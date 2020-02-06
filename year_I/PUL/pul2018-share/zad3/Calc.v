`default_nettype none

`define CLOG2(x) \
   (x <= 2) ? 1 : \
   (x <= 4) ? 2 : \
   (x <= 8) ? 3 : \
   (x <= 16) ? 4 : \
   (x <= 32) ? 5 : \
   (x <= 64) ? 6 : \
   (x <= 128) ? 7 : \
   (x <= 256) ? 8 : \
   -1


module synchronize_inputs(clk, inputs, sync_inputs);
    parameter BITS = 10;
    parameter SYNC_LEVEL = 2;

    input wire clk;

    input wire [BITS - 1:0] inputs;
    output wire [BITS - 1:0] sync_inputs;

    reg [BITS - 1:0] sync[SYNC_LEVEL:0];

    integer j;
    initial begin
        for (j = 0; j <= SYNC_LEVEL; j = j + 1 )
            sync[j] = 0;
    end

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


module digit_lcd_segments(input wire displayNothing, input wire [3:0] number, output reg [6:0] seg);
    always @(number, displayNothing) begin
        if (displayNothing) begin
            seg <= 7'h3f;
        end
        else begin
            case (number)
                4'h0: begin
                    seg <= 7'h40;
                end
                4'h1: begin
                    seg <= 7'h79;
                end
                4'h2: begin
                    seg <= 7'h24;
                end
                4'h3: begin
                    seg <= 7'h30;
                end
                4'h4: begin
                    seg <= 7'h19;
                end
                4'h5: begin
                    seg <= 7'h12;
                end
                4'h6: begin
                    seg <= 7'h02;
                end
                4'h7: begin
                    seg <= 7'h78;
                end
                4'h8: begin
                    seg <= 7'h00;
                end
                4'h9: begin
                    seg <= 7'h10;
                end
                4'ha: begin
                    seg <= 7'h08;
                end
                4'hb: begin
                    seg <= 7'h03;
                end
                4'hc: begin
                    seg <= 7'h46;
                end
                4'hd: begin
                    seg <= 7'h21;
                end
                4'he: begin
                    seg <= 7'h06;
                end
                4'hf: begin
                    seg <= 7'h0e;
                end
                default: begin
                    seg <= 7'h3f;
                end
            endcase
        end
    end
endmodule


module screen_lcd(
    input wire clk,
    input wire displayNothing,
    input wire [15:0] numIn,
    output wire [6:0] seg,
    output reg [3:0] an
    );
    localparam DISPLAY_QUANT = 1024;

    reg [15:0] displayState, num;
    reg [3:0] current_digit;

    initial begin
        displayState = 0;
        num = 0;
    end

    wire [6:0] segAsync;

    synchronize_inputs #(.BITS(7), .SYNC_LEVEL(3)) sync(.inputs(segAsync), .clk(clk), .sync_inputs(seg));
    digit_lcd_segments d_sgms(.displayNothing(displayNothing), .number(current_digit), .seg(segAsync));

    always @(posedge clk) begin
        if (displayState == 0)
            num <= numIn;
        current_digit <= num[4 * (displayState[15:14]) +: 4];

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

module divider(clk, stb, A, Q, R, B, done);
    localparam BITS = 32;
    localparam run=0, done_div=1;

    input wire clk, stb;
    input wire [BITS-1:0] A;
    input wire [BITS-1:0] B;

    reg [`CLOG2(BITS):0] ctr;
    output reg [BITS-1:0] Q, R;
    output reg done;

    reg [2*BITS-1:0] tempB;
    reg state;
    reg [BITS - 1: 0] Q_temp, R_temp;

    always @(posedge clk or posedge stb) begin
        if (stb) begin
            state <= run;
            ctr <= 0;
            tempB <= (!B[BITS - 1] ? B : ((~B) + 1)) << BITS;
            Q_temp <= 0;
            R_temp <= !A[BITS - 1] ? A : ~A + 1;
            done <= 0;
        end
        else begin
            case (state)
            run: begin
                if (R_temp >= tempB) begin
                    R_temp <= R_temp - tempB;
                    Q_temp <= (Q_temp << 1) + 1;
                end
                else
                    Q_temp <= (Q_temp << 1);
                tempB <= tempB >> 1;
                ctr <= ctr + 1;

                if (ctr == BITS) begin
                    state <= done_div;
                end
            end
            done_div: begin
                if (A[BITS - 1]) begin
                    R <= (~R_temp) + 1;
                end else begin
                    R <= R_temp;
                end
                if ((A[BITS - 1] && !B[BITS - 1]) || (!A[BITS - 1] && B[BITS - 1])) begin
                    Q <= (~Q_temp) + 1;
                end else begin
                    Q <= Q_temp;
                end

                done <= 1;
            end
            endcase
        end
    end
endmodule


module Calc (
    input wire clk,
    input wire [3:0] btn,
    input wire [7:0] sw,
    output wire [7:0] led,
    output wire [3:0] an,
    output wire [6:0] seg
    );

    localparam ADD = 8'h00;
    localparam SUB = 8'h01;
    localparam MUL = 8'h02;
    localparam DIV = 8'h03;
    localparam REM = 8'h04;

    localparam POP_CMD_0 = 8'h5;
    localparam DUP_CMD = 8'h6;
    localparam SWAP_CMD_0 = 8'h7;

    localparam PUSH_CMD = 8'h8;
    localparam EXTEND_CMD = 8'h9;

    localparam POP_CMD_1 = 8'ha;
    localparam POP_CMD_2 = 8'hb;
    localparam SWAP_CMD_1 = 8'hc;
    localparam SWAP_CMD_2 = 8'hd;
    localparam SWAP_CMD_3 = 8'he;

    localparam BINARY_OPERATION_0 = 8'hf;
    localparam BINARY_OPERATION_1 = 8'h10;
    localparam BINARY_OPERATION_2 = 8'h11;
    localparam BINARY_OPERATION_3 = 8'h12;
    localparam BINARY_OPERATION_4 = 8'h13;

    localparam IDLE = 8'h14;
    localparam DIVIDER = 8'h15;

    reg [2:0] binary_op;
    reg [7:0] state;

    reg [31:0] RAM_BUF [0:511], numRead, numWrite, stackTop;

    reg [15:0] numToDisplay;
    reg [8:0] ramIdx, stackSize;

    wire [31:0] Q, R;
    wire [7:0] hidNum;

    reg stb_divider, write, error;

    wire [11:0] inputs, sync_inputs;
    wire [3:0] sync_btn;
    reg [3:1] btn_pressed;
    wire divider_done;

    assign inputs[3:0] = btn[3:0];
    assign inputs[11:4] = sw[7:0];

    synchronize_inputs #(.BITS(12), .SYNC_LEVEL(3)) sync(.inputs(inputs), .clk(clk), .sync_inputs(sync_inputs));
    screen_lcd screen(.clk(clk), .displayNothing(stackSize == 0), .numIn(numToDisplay), .an(an), .seg(seg));
    divider div(.A(numRead), .B(stackTop), .Q(Q), .R(R), .clk(clk), .stb(stb_divider), .done(divider_done));

    assign sync_btn = sync_inputs[3:0];
    assign hidNum = sync_inputs[11:4];

    initial begin
        write = 0;
        error = 0;

        numWrite = 0;
        numRead = 0;
        ramIdx = 0;

        stackTop = 0;
        stackSize = 0;

        stb_divider = 1;
        state = IDLE;
    end

    always @(posedge clk) begin
        if (write)
            RAM_BUF[ramIdx] <= numWrite;
        else
            numRead <= RAM_BUF[ramIdx];

        write <= 0;

        if (sync_btn[0])
            numToDisplay <= stackTop[31:16];
        else
            numToDisplay <= stackTop[15:0];

        case (state)
            IDLE: begin
                case (sync_btn)
                    4'b0010: begin
                        if (!btn_pressed[1]) begin
                            btn_pressed[1] <= 1;
                            state <= PUSH_CMD;
                        end
                    end

                    4'b0100: begin
                        if (!btn_pressed[2]) begin
                            btn_pressed[2] <= 1;
                            state <= EXTEND_CMD;
                        end
                    end

                    4'b1000: begin
                        if (!btn_pressed[3]) begin
                            btn_pressed[3] <= 1;

                            case (sync_inputs[6:4])
                                3'b000: begin
                                    binary_op <= ADD;
                                    state <= BINARY_OPERATION_0;
                                end
                                3'b001: begin
                                    binary_op <= SUB;
                                    state <= BINARY_OPERATION_0;
                                end
                                3'b010: begin
                                    binary_op <= MUL;
                                    state <= BINARY_OPERATION_0;
                                end
                                3'b011: begin
                                    if (stackTop == 0) begin
                                        state <= IDLE;
                                        error <= 1;
                                    end
                                    else begin
                                        binary_op <= DIV;
                                        state <= BINARY_OPERATION_0;
                                    end
                                end
                                3'b100: begin
                                    if (stackTop == 0) begin
                                        state <= IDLE;
                                        error <= 1;
                                    end
                                    else begin
                                        binary_op <= REM;
                                        state <= BINARY_OPERATION_0;
                                    end
                                end
                                3'b101:
                                    state <= POP_CMD_0;
                                3'b110:
                                    state <= DUP_CMD;
                                3'b111:
                                    state <= SWAP_CMD_0;
                            endcase
                        end
                    end

                    4'b1001: begin
                        error <= 0;
                        stackSize <= 0;
                        stackTop <= 0;
                        write <= 0;
                        stb_divider <= 1;
                        state <= IDLE;
                    end

                    default: begin
                        btn_pressed <= 0;
                    end
                endcase
            end

             PUSH_CMD: begin
                 if (stackSize == 511) begin
                     state <= IDLE;
                     error <= 1;
                 end
                 else begin
                     stackTop <= hidNum;

                     ramIdx <= stackSize;
                     numWrite <= hidNum;
                     write <= 1;
                     state <= IDLE;
                     error <= 0;

                     stackSize <= stackSize + 1;
                 end
             end

             EXTEND_CMD: begin
                 if (stackSize == 0) begin
                     state <= IDLE;
                     error  <= 1;
                 end
                 else begin
                     stackTop <= (stackTop << 8) + hidNum;
                     ramIdx <= stackSize - 1;
                     numWrite <= (stackTop << 8) + hidNum;
                     write <= 1;
                     state <= IDLE;
                     error <= 0;
                 end
             end

             POP_CMD_0: begin
                if (stackSize == 0) begin
                  state <= IDLE;
                  error <= 1;
                end
                else begin
                  if (stackSize == 1) begin
                      state <= IDLE;
                      error <= 0;
                  end
                  else begin
                      ramIdx <= stackSize - 2;
                      write <= 0;
                      state <= POP_CMD_1;
                  end
                  stackSize <= stackSize - 1;
                end
             end

             POP_CMD_1: begin
                state <= POP_CMD_2;
             end

             POP_CMD_2: begin
                 stackTop <= numRead;
                 state <= IDLE;
                 error <= 0;
             end

             DUP_CMD: begin
                 if (stackSize == 0 || stackSize == 511) begin
                     state <= IDLE;
                     error <= 1;
                 end
                 else begin
                     ramIdx <= stackSize;
                     numWrite <= stackTop;
                     write <= 1;
                     state <= IDLE;
                     error <= 0;
                     stackSize <= stackSize + 1;
                 end
             end

             SWAP_CMD_0: begin
                  if (stackSize < 2) begin
                      state <= IDLE;
                      error <= 1;
                  end
                  else begin
                      ramIdx <= stackSize - 2;
                      write <= 0;
                      state <= SWAP_CMD_1;
                  end
             end

             SWAP_CMD_1: begin
                state <= SWAP_CMD_2;
             end

             SWAP_CMD_2: begin
                  stackTop <= numRead;
                  numWrite <= stackTop;
                  write <= 1;
                  state <= SWAP_CMD_3;
             end

             SWAP_CMD_3: begin
                  numWrite <= stackTop;
                  ramIdx <= stackSize - 1;
                  write <= 1;
                  state <= IDLE;
                  error <= 0;
             end

             BINARY_OPERATION_0: begin
                 if (stackSize < 2) begin
                     state <= IDLE;
                     error <= 1;
                 end
                 else begin
                     ramIdx <= stackSize - 2;
                     write <= 0;
                     state <= BINARY_OPERATION_1;
                 end
             end

             BINARY_OPERATION_1: begin
                state <= BINARY_OPERATION_2;
             end

             BINARY_OPERATION_2: begin
                 state <= BINARY_OPERATION_3;

                 case (binary_op)
                     ADD: begin
                         stackTop <= stackTop + numRead;
                     end
                     SUB: begin
                         stackTop <= numRead - stackTop;
                     end
                     MUL: begin
                         stackTop <= numRead * stackTop;
                     end
                     DIV: begin
                         state <= DIVIDER;
                         stb_divider <= 0;
                     end
                     REM: begin
                         state <= DIVIDER;
                         stb_divider <= 0;
                     end
                     default: begin
                         state <= IDLE;
                         error <= 1;
                     end
                 endcase
             end

             BINARY_OPERATION_3: begin
                 if (binary_op == DIV)
                     stackTop <= Q;
                 if (binary_op == REM)
                     stackTop <= R;
                 stb_divider <= 1;
                 state <= BINARY_OPERATION_4;
             end

             BINARY_OPERATION_4: begin
                 numWrite <= stackTop;
                 ramIdx <= stackSize - 2;
                 write <= 1;
                 stackSize <= stackSize - 1;
                 state <= IDLE;
                 error <= 0;
             end

             DIVIDER: begin
                if (divider_done)
                    state <= BINARY_OPERATION_3;
             end

             default: begin
                 state <= IDLE;
                 error <= 1;
             end
        endcase
    end

    assign led[6:0] = stackSize[6:0];
    assign led[7] = error;

endmodule
