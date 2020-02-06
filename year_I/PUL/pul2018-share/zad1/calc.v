`default_nettype none

module divider(a, b, quot, rem);

parameter BITS = 4;

input wire [BITS-1:0] a;
input wire [BITS-1:0] b;

output wire [BITS-1:0] quot;
output wire [BITS-1:0] rem;

wire [BITS-1:0] tempRes[BITS:0];

assign tempRes[BITS] = a;

genvar i;
generate

    for (i = BITS - 1; i >= 0; i = i - 1) begin : gen_divider
        assign quot[i] = (tempRes[i + 1] >> i) >= b;
        assign tempRes[i] = quot[i] ? (tempRes[i + 1] - (b << i)) : tempRes[i + 1];
    end

endgenerate

assign rem = tempRes[0];

endmodule

module calc(
    input [7:0] sw,
    input [3:0] btn,
    output reg [7:0] led
    );

wire cmp = sw[7:4] > sw[3:0];
wire [3:0] quot1;
wire [3:0] rem1;
reg [3:0] a;
reg [3:0] b;

divider d(.a(a), .b(b), .quot(quot1), .rem(rem1));

always @(*) begin
    a <= sw[7:4];
    b <= sw[3:0];

    case (btn)
    4'b0000: begin
        led <= 8'b00000000;
    end
    4'b0001: begin
        led[7:4] <= a + b;
        led[3:0] <= a - b;
    end
    4'b0010: begin
        led[7:4] <= cmp ? b : a;
        led[3:0] <= cmp ? a : b;
    end
    4'b0100: begin
        led <= b * a;
    end
    4'b1000: begin
        led[7:4] <= quot1;
        led[3:0] <= rem1;
    end
    default : begin
        led[7:4] <= a;
        led[3:0] <= b;
    end
    endcase
end

endmodule
