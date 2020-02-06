`default_nettype none


module synchronize_inputs(clk, inputs, sync_inputs);
    parameter BITS = 10;
    parameter SYNC_LEVEL = 2;

    input wire clk;

    input wire [BITS - 1:0] inputs;
    output wire [BITS - 1:0] sync_inputs;

    reg [BITS - 1:0] sync[SYNC_LEVEL:0];

    always @(posedge clk) begin
        sync[SYNC_LEVEL] <= inputs;
    end

    genvar i;
    generate
        for (i = SYNC_LEVEL - 1; i >= 0; i = i - 1 ) begin : gen_sync
            always @(posedge clk) begin
                sync[i] <= sync[i + 1];
            end
        end
    endgenerate

    assign sync_inputs = sync[0];
endmodule


module digit_lcd_segments(input wire [3:0] number, output reg [6:0] seg);
    always @(number) begin
        case (number)
            4'b0000: begin
                seg <= 7'h40;
            end
            4'b0001: begin
                seg <= 7'h79;
            end
            4'b0010: begin
                seg <= 7'h24;
            end
            4'b0011: begin
                seg <= 7'h30;
            end
            4'b0100: begin
                seg <= 7'h19;
            end
            4'b0101: begin
                seg <= 7'h12;
            end
            4'b0110: begin
                seg <= 7'h02;
            end
            4'b0111: begin
                seg <= 7'h78;
            end
            4'b1000: begin
                seg <= 7'h00;
            end
            4'b1001: begin
                seg <= 7'h10;
            end
            default: begin
                seg <= 7'h7f;
            end
        endcase
    end
endmodule


module divider(clk, num, div, digit);
    parameter IN_BITS = 4;
    parameter OUT_BITS = 4;

    input wire clk;
    input wire [IN_BITS-1:0] num;
    wire [IN_BITS-1:0] b = 10;

    output reg [OUT_BITS-1:0] div;
    output wire [3:0] digit;

    wire [IN_BITS-1:0] tempRes[OUT_BITS:0];

    assign tempRes[OUT_BITS] = num;

    genvar i;
    generate
        for (i = OUT_BITS - 1; i >= 0; i = i - 1) begin : gen_divider
            always @(posedge clk) begin
                div[i] <= (tempRes[i + 1] >> i) >= b;
            end
            assign tempRes[i] = div[i] ? (tempRes[i + 1] - (b << i)) : tempRes[i + 1];
        end
    endgenerate

    assign digit = tempRes[0];
endmodule


module screen_lcd(
    input wire clk,
    input wire [13:0] num,
    output wire [6:0] seg,
    output reg [3:0] an
    );
    localparam DISPLAY_QUANT = 1024;

    reg [15:0] displayState = 0;
    wire [3:0] digits[3:0];
    reg [3:0] current_digit;
    wire [13:0] divs0;

    synchronize_inputs #(.BITS(14), .SYNC_LEVEL(2)) sync(.clk(clk), .inputs(num), .sync_inputs(divs0));

    wire [9:0] divs1; // 0 <= num <= 9999 means we can only get 10 non-zero bits
    divider #(.IN_BITS(14), .OUT_BITS(10)) d0(.clk(clk), .num(divs0), .div(divs1), .digit(digits[0]));
    wire [6:0] divs2;
    divider #(.IN_BITS(10), .OUT_BITS(7)) d1(.clk(clk), .num(divs1), .div(divs2), .digit(digits[1]));

    divider #(.IN_BITS(7), .OUT_BITS(4)) d(.clk(clk), .num(divs2), .div(digits[3]), .digit(digits[2]));

    digit_lcd_segments d_sgms(.number(current_digit), .seg(seg));

    always @(posedge clk) begin
        current_digit <= digits[displayState[15:14]];

        if (displayState[13:0] < DISPLAY_QUANT) begin
            an <= 4'b1111;
        end
        else begin
            if (displayState[13:0] < 15 * DISPLAY_QUANT) begin
                an <= 4'b1111 ^ (1 << displayState[15:14]);
            end
            else begin
                an <= 4'b1111;
            end
        end

        displayState <= displayState + 1;
    end
endmodule


module ticker(
    input wire clk,
    input wire reset,
    input wire [4:0] freq,
    input wire stop,
    input wire countUp,
    output wire endReached,
    output reg [13:0] num
    );
    localparam SCREEN_MAX = 9999;
    localparam SCREEN_MIN = 0;

    reg [31:0] ctr = 0;
    wire countSaturated = (num == SCREEN_MAX && countUp) || (num == SCREEN_MIN && !countUp);
    assign endReached = countSaturated && !stop;

    always @(posedge clk) begin
        if (reset) begin
            ctr <= 0;
            if (countUp)
                num <= SCREEN_MIN;
            else
                num <= SCREEN_MAX;

        end
        else begin
            if (!stop) begin
                if (ctr >= ((1 << freq) - 1)) begin
                    ctr <= 0;
                    if (!countSaturated) begin
                        if (countUp)
                            num <= num + 1;
                        else
                            num <= num - 1;
                    end
                end
                else begin
                    ctr <= ctr + 1;
                end
            end
        end
    end
endmodule

module stoper(
    input wire swclk,
    input wire uclk,
    input wire mclk,
    input wire [4:0] sw,
    input wire [3:0] btn,
    output wire [2:0] led,
    output wire [3:0] an,
    output wire [6:0] seg
    );

    wire [9:0] inputs, sync_inputs;
    wire [13:0] num;
    wire swclkSynced;
    wire clk;

    BUFGMUX buffer(.I0(mclk), .I1(uclk), .S(swclkSynced), .O(clk));

    assign inputs[3:0] = btn[3:0];
    assign inputs[8:4] = sw[4:0];
    assign inputs[9] = swclk;

    synchronize_inputs #(.BITS(10), .SYNC_LEVEL(2)) sync(.inputs(inputs), .clk(clk), .sync_inputs(sync_inputs));

    wire [3:0] sync_btn;
    wire [4:0] freq;

    assign sync_btn = sync_inputs[3:0];
    assign freq = sync_inputs[8:4];
    assign swclkSynced = sync_inputs[9];

    // Outputs of the case circuit
    reg stop = 1, countUp = 1, reset = 0;

    wire endReached;

    ticker tick(.clk(clk), .freq(freq), .reset(reset), .stop(stop), .countUp(countUp), .endReached(endReached), .num(num));
    screen_lcd sc(.clk(clk), .num(num), .an(an), .seg(seg));

    always @(posedge clk) begin
        case (sync_btn)
            4'b1000: begin
                reset <= 1;
                stop <= 1;
                countUp <= 1;
            end

            4'b0100: begin
                reset <= 0;
                stop <= 1;
            end

            4'b0010: begin
                reset <= 0;
                stop <= 0;
                countUp <= 1;
            end

            4'b0001: begin
                reset <= 0;
                stop <= 0;
                countUp <= 0;
            end
            default: begin
                reset <= 0;
            end
        endcase
    end

    assign led[0] = countUp == 0 && !endReached;
    assign led[1] = countUp == 1 && !endReached;
    assign led[2] = endReached;
endmodule
