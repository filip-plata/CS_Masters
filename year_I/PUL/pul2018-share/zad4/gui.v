`default_nettype none

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
            sync[j] = 1;
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


module blit_copy_order(
    input wire [15:0] T_X,
    input wire [15:0] T_Y,
    input wire [15:0] S_X,
    input wire [15:0] S_Y,
    output reg forward,
    output reg ignore);

    always @(*) begin
        ignore = 0;
        if (T_Y > S_Y)
            forward = 0;
        else begin
        if (T_Y < S_Y)
            forward = 1;
        else begin
            if (T_X < S_X)
                forward = 1;
            else begin
                if (T_X == S_X) begin
                    forward = 1'bx;
                    ignore = 1;
                end
                else begin
                    forward = 0;
                end
            end
        end
        end
    end
endmodule


module compute_next_parameters(
    input wire signed [15:0] operation_X,
    input wire signed [15:0] operation_Y,
    input wire [15:0] width,
    input wire [15:0] height,
    input forward,

    output reg [15:0] next_operation_X,
    output reg [15:0] next_operation_Y,
    output wire done
    );

    wire [15:0] candidate_next_X_forward = operation_X + 1;
    wire [15:0] candidate_next_Y_forward = operation_Y + 1;
    wire signed [15:0] candidate_next_X_backward = operation_X - 1;
    wire signed [15:0] candidate_next_Y_backward = operation_Y - 1;

    assign done = forward ? operation_Y >= height : operation_Y < 0;

    always @(*) begin

    if (forward) begin
        if (candidate_next_X_forward >= width) begin
            next_operation_X = 0;
            next_operation_Y = candidate_next_Y_forward;
        end else begin
            next_operation_Y = operation_Y;
            next_operation_X = candidate_next_X_forward;
        end
    end else begin
        if (candidate_next_X_backward < 0) begin
            next_operation_X = width - 1;
            next_operation_Y = candidate_next_Y_backward;
        end else begin
            next_operation_Y = operation_Y;
            next_operation_X = candidate_next_X_backward;
        end
    end

    end

endmodule


module gui (
    input wire CLK,

    input wire uclk,

    output reg HSYNC,
    output reg VSYNC,
    output reg [2:0] VGAR,
    output reg [2:0] VGAG,
    output reg [2:1] VGAB,

    input wire EPP_ASTB,
    input wire EPP_DSTB,
    inout wire [7:0] EPP_D,
    input wire EPP_WR,
    output reg EPP_WAIT
    );

    localparam RESOLUTION_WIDTH = 320;
    localparam RESOLUTION_HEIGHT = 200;

    localparam EPP_INIT = 2'b00;
    localparam EPP_WAIT_ST = 2'b01;

    localparam CARD_IDLE = 4'h0;
    localparam CARD_FILL = 4'h1;
    localparam CARD_BLIT = 4'h2;
    localparam CARD_BLIT_EXEC = 4'h3;
    localparam CARD_WRITE = 4'h4;
    localparam CARD_ADVANCE_P_1 = 4'h5;

    localparam ADDR_X_1_LOW = 4'h0;
    localparam ADDR_X_1_HIGH = 4'h1;
    localparam ADDR_Y_1_LOW = 4'h2;
    localparam ADDR_Y_1_HIGH = 4'h3;

    localparam ADDR_X_2_LOW = 4'h4;
    localparam ADDR_X_2_HIGH = 4'h5;
    localparam ADDR_Y_2_LOW = 4'h6;
    localparam ADDR_Y_2_HIGH = 4'h7;

    localparam ADDR_WIDTH_LOW = 4'h8;
    localparam ADDR_WIDTH_HIGH = 4'h9;
    localparam ADDR_HEIGHT_LOW = 4'ha;
    localparam ADDR_HEIGHT_HIGH = 4'hb;

    localparam ADDR_BLIT = 4'hc;
    localparam ADDR_FILL = 4'hd;
    localparam ADDR_ACC = 4'he;
    localparam ADDR_STATUS = 4'hf;

    reg rdata_oe, fill_color, ramWrite, forward_op;
    reg [3:0] card_register, card_state;
    reg signed [15:0] X_1, Y_1, X_2, Y_2, width, height, operation_X, operation_Y;
    wire [15:0] next_operation_X, next_operation_Y;
    wire [10:0] epp_input, epp_synced;
    reg [1:0] epp_state;
    reg astb, dstb, wr;
    reg [7:0] wdata, rdata;
    reg RAM [0:65535], numWrite, numRead;
    wire [15:0] target_idx, first_idx, source_idx;
    reg [15:0] ramIdx;
    wire op_done, blip_forward, blip_ignore;
    reg [3:0] ctr;

    assign target_idx = X_1 + operation_X + (Y_1 + operation_Y) * RESOLUTION_WIDTH;
    assign source_idx = X_2 + operation_X + (Y_2 + operation_Y) * RESOLUTION_WIDTH;
    assign first_idx = X_1 + Y_1 * RESOLUTION_WIDTH;

    wire clk_;

    DCM_SP #(
        .CLKFX_DIVIDE(14),
        .CLKFX_MULTIPLY(11)
    ) display_dcm (
        .CLKIN(uclk),
        .CLKFX(clk_),
        .RST(0)
    );

    initial begin
        X_1 <= 0;
        X_2 <= 0;
        Y_1 <= 0;
        Y_2 <= 0;
        operation_X <= 0;
        operation_Y <= 0;
        width <= 0;
        height <= 0;
        fill_color <= 0;
        rdata_oe <= 0;

        ramWrite <= 0;
        ramIdx <= 0;
        numWrite <= 0;
        numRead <= 0;

        epp_state <= EPP_INIT;
        wdata <= 0;
        astb <= 1;
        dstb <= 1;
        ctr <= 0;

        card_state <= CARD_IDLE;
    end

    assign epp_input[0] = EPP_ASTB;
    assign epp_input[1] = EPP_DSTB;
    assign epp_input[9:2] = EPP_D;
    assign epp_input[10] = EPP_WR;

    assign EPP_D = rdata_oe ? rdata : 8'hzz;

    synchronize_inputs #(.BITS(11), .SYNC_LEVEL(2)) sync(.inputs(epp_input), .clk(CLK), .sync_inputs(epp_synced));

    blit_copy_order blip_ord(.S_X(X_2), .S_Y(Y_2), .T_X(X_1), .T_Y(Y_1), .forward(blip_forward), .ignore(blip_ignore));

    compute_next_parameters nexts(.operation_X(operation_X), .operation_Y(operation_Y), .width(width),
                                  .height(height), .forward(forward_op), .done(op_done),
                                  .next_operation_X(next_operation_X), .next_operation_Y(next_operation_Y));

    always @(*) begin
        ramWrite = 0;
        ramIdx = first_idx + ctr;
        numWrite = 8'hxx;

        case (card_state)
            CARD_WRITE: begin
                if (!ctr[3]) begin
                    ramWrite = 1;
                    numWrite = wdata[ctr[2:0]];
                end
            end

            CARD_FILL: begin
                ramIdx = target_idx;
                if (!op_done) begin
                    ramWrite = 1;
                    numWrite = fill_color;
                end
            end

            CARD_BLIT: begin
                ramIdx = source_idx;
            end

            CARD_BLIT_EXEC: begin
                ramIdx = target_idx;
                ramWrite = 1;
                numWrite = numRead;
            end
        endcase
    end

    always @(posedge CLK) begin
        // RAM
        if (ramWrite)
            RAM[ramIdx] <= numWrite;
        numRead <= RAM[ramIdx];

        // Card
        case (card_state)
            CARD_WRITE: begin
                if (!ctr[3])
                    ctr <= ctr + 1;
                else begin
                    card_state <= CARD_ADVANCE_P_1;
                end
            end

            CARD_BLIT: begin
                if (op_done)
                    card_state <= CARD_IDLE;
                else begin
                    card_state <= CARD_BLIT_EXEC;
                end
            end

            CARD_BLIT_EXEC: begin
                card_state <= CARD_BLIT;
                operation_X <= next_operation_X;
                operation_Y <= next_operation_Y;
            end

            CARD_FILL: begin
                if (op_done)
                    card_state <= CARD_IDLE;
                else begin
                    operation_X <= next_operation_X;
                    operation_Y <= next_operation_Y;
                end
            end

            CARD_ADVANCE_P_1: begin
                if (X_1 == RESOLUTION_WIDTH - 8) begin
                    X_1 <= 0;

                    if (Y_1 == RESOLUTION_HEIGHT - 1) begin
                        Y_1 <= 0;
                    end else
                        Y_1 <= Y_1 + 1;
                end else
                    X_1 <= X_1 + 8;
                card_state <= CARD_IDLE;
            end
        endcase

        // EPP
        astb <= epp_synced[0];
        dstb <= epp_synced[1];
        wdata <= epp_synced[9:2];
        wr <= epp_synced[10];

        case (epp_state)
            EPP_INIT: begin
                EPP_WAIT <= 0;

                if (!astb && !wr) begin
                    if (!(|wdata[7:4])) begin
                        card_register <= wdata;
                    end
                    epp_state <= EPP_WAIT_ST;
                end

                if (!astb && wr) begin
                    epp_state <= EPP_WAIT_ST;
                    rdata <= card_register;
                end

                if (!dstb && !wr) begin
                    epp_state <= EPP_WAIT_ST;
                    case (card_register)
                    ADDR_X_1_LOW: begin
                        X_1[7:0] <= wdata;
                    end
                    ADDR_X_1_HIGH: begin
                        X_1[15:8] <= wdata;
                    end
                    ADDR_Y_1_LOW: begin
                        Y_1[7:0] <= wdata;
                    end
                    ADDR_Y_1_HIGH: begin
                        Y_1[15:8] <= wdata;
                    end

                    ADDR_X_2_LOW: begin
                        X_2[7:0] <= wdata;
                    end
                    ADDR_X_2_HIGH: begin
                        X_2[15:8] <= wdata;
                    end
                    ADDR_Y_2_LOW: begin
                        Y_2[7:0] <= wdata;
                    end
                    ADDR_Y_2_HIGH: begin
                        Y_2[15:8] <= wdata;
                    end

                    ADDR_WIDTH_LOW: begin
                        width[7:0] <= wdata;
                    end
                    ADDR_WIDTH_HIGH: begin
                        width[15:8] <= wdata;
                    end
                    ADDR_HEIGHT_LOW: begin
                        height[7:0] <= wdata;
                    end
                    ADDR_HEIGHT_HIGH: begin
                        height[15:8] <= wdata;
                    end

                    ADDR_BLIT: begin
                        if (card_state == CARD_IDLE && !blip_ignore) begin
                            card_state <= CARD_BLIT;
                            operation_X <= blip_forward ? 0 : width - 1;
                            operation_Y <= blip_forward ? 0 : height - 1;
                            forward_op <= blip_forward;
                        end
                    end
                    ADDR_FILL: begin
                        fill_color <= wdata[0];
                        if (card_state == CARD_IDLE) begin
                            forward_op <= 1;
                            card_state <= CARD_FILL;
                            operation_X <= 0;
                            operation_Y <= 0;
                        end
                    end
                    ADDR_ACC: begin
                        if (card_state == CARD_IDLE && X_1[2:0] == 0) begin
                            if (!ctr[3]) begin
                                epp_state <= EPP_INIT;
                                card_state <= CARD_WRITE;
                            end else begin
                                epp_state <= EPP_WAIT_ST;
                            end
                        end
                    end
                    endcase
                end

                if (!dstb && wr) begin
                    rdata_oe <= 1;
                    epp_state <= EPP_WAIT_ST;

                    case (card_register)
                    ADDR_X_1_LOW: begin
                        rdata <= X_1[7:0];
                    end
                    ADDR_X_1_HIGH: begin
                        rdata <= X_1[15:8];
                    end
                    ADDR_Y_1_LOW: begin
                        rdata <= Y_1[7:0];
                    end
                    ADDR_Y_1_HIGH: begin
                        rdata <= Y_1[15:8];
                    end

                    ADDR_X_2_LOW: begin
                        rdata <= X_2[7:0];
                    end
                    ADDR_X_2_HIGH: begin
                        rdata <= X_2[15:8];
                    end
                    ADDR_Y_2_LOW: begin
                        rdata <= Y_2[7:0];
                    end
                    ADDR_Y_2_HIGH: begin
                        rdata <= Y_2[15:8];
                    end

                    ADDR_WIDTH_LOW: begin
                        rdata <= width[7:0];
                    end
                    ADDR_WIDTH_HIGH: begin
                        rdata <= width[15:8];
                    end
                    ADDR_HEIGHT_LOW: begin
                        rdata <= height[7:0];
                    end
                    ADDR_HEIGHT_HIGH: begin
                        rdata <= height[15:8];
                    end

                    ADDR_ACC: begin
                        if (card_state == CARD_IDLE && X_1[2:0] == 0) begin
                            if (ctr != 0) begin
                                rdata[ctr - 1] <= numRead;
                            end

                            if (!ctr[3]) begin
                                epp_state <= EPP_INIT;
                                ctr <= ctr + 1;
                            end else
                                card_state <= CARD_ADVANCE_P_1;
                        end
                    end
                    ADDR_STATUS: begin
                        rdata <= (card_state == CARD_IDLE) ? 0 : 1;
                    end
                    endcase
                end
            end

            EPP_WAIT_ST: begin
                ctr <= 0;
                EPP_WAIT <= 1;
                if (astb && dstb) begin
                    rdata_oe <= 0;
                    epp_state <= EPP_INIT;
                end
            end
        endcase
    end

    // Display
    localparam MAX_HC = 800;
    localparam MAX_VC = 449;
    localparam HS_START = RESOLUTION_WIDTH + 8;
    localparam HS_END = HS_START + 48;
    localparam VS_START = RESOLUTION_HEIGHT + 6;
    localparam VS_END = VS_START + 1;

    wire DE1;
    reg [9:0] hc;
    reg [8:0] vc;

    reg [8:0] hc1;
    reg [7:0] vc1;

    assign DE1 = (hc1 < RESOLUTION_WIDTH && vc1 < RESOLUTION_HEIGHT);

    reg chr1;

    wire [15:0] ramaddr0;

    always @(posedge clk_) begin
        if (hc == MAX_HC - 1) begin
            hc <= 0;
            if (vc == MAX_VC - 1)
                vc <= 0;
            else
                vc <= vc + 1;
        end else
            hc <= hc + 1;
    end

    assign ramaddr0 = ((hc[9:1]) + (vc[8:1]) * RESOLUTION_WIDTH);

    always @(posedge clk_) begin
    	hc1 <= hc[9:1];
    	vc1 <= vc[8:1];

    	chr1 <= RAM[ramaddr0];

    	VGAR <= DE1 ? chr1 ? 7 : 0 : 0;
    	VGAG <= DE1 ? chr1 ? 7 : 0 : 0;
    	VGAB <= DE1 ? chr1 ? 3 : 0 : 0;
    	HSYNC <= (hc1 >= HS_START && hc1 < HS_END);
    	VSYNC <= (vc1 >= VS_START && vc1 < VS_END);
    end
endmodule