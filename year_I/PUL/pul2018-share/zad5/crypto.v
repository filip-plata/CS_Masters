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


module RNG (
    input wire clk,
    input wire reset,
    output reg [7:0] out);

    wire  linear_feedback;

    assign linear_feedback =  ! (out[7] ^ out[3]);

    initial out = 0;

    always @(posedge clk) begin
        if (reset) begin
            out <= 8'b0 ;
        end else
            out <= {out[6],out[5],
                   out[4],out[3],
                   out[2],out[1],
                   out[0], linear_feedback};
    end
endmodule


module arithmetic_coprocesor(
    input wire clk,
    input wire stb,
    input wire [2:0] operation,
    input wire [31:0] in_num,
    input wire [7:0] addr,
    output reg [31:0] out_num,
    output reg [1:0] bool_res,
    output reg done
    );
    localparam DONE = 0;
    localparam LOAD_OP_WRITE = 1;
    localparam CMP_3 = 2;
    localparam READ_RES = 3;
    localparam ZERO_VARS = 4;
    localparam MULTIPLY_START = 5;
    localparam MULTIPLY_WRITE_1 = 6;
    localparam MULTIPLY_DIGITS_MUL = 7;
    localparam MULTIPLY_WRITE_2 = 8;
    localparam INIT = 9;
    localparam SHIFT_RES_1 = 10;
    localparam SHIFT_RES_2 = 11;
    localparam CMP_1 = 12;
    localparam CMP_2 = 13;
    localparam SUBTRACT_1 = 14;
    localparam SUBTRACT_2 = 15;
    localparam SUBTRACT_3 = 16;
    localparam COPY_RES_LOWER_TO_OP_2_1 = 17;
    localparam COPY_RES_LOWER_TO_OP_2_2 = 18;
    localparam MULTIPLY_DIGITS_PREPARE = 19;
    localparam MULTIPLY_DIGITS_MUL_1 = 20;
    localparam SUBTRACT_4 = 21;
    localparam MULTIPLY_DIGITS_PREPARE_1 = 22;

    reg ramWrite, digit_multiply_stall;
    reg [9:0] offsetAddr, ramAddr;
    reg [6:0] offsetAddrMul1, offsetAddrMul2;
    reg [31:0] RAM1 [0:1023], numWrite, numRead, op1;
    reg [15:0] op2;
    reg [63:0] mulTemp;
    reg [47:0] mulTemp1;
    reg [31:0] subTemp;
    reg carry, next_carry, arithmetic_do_sub, addition_ret_to_mul;
    reg [1:0] sub_op_1, sub_op_2;

    reg [4:0] state;

    initial begin
        state <= INIT;
        mulTemp <= 0;
        subTemp <= 0;
        offsetAddr <= 0;
        arithmetic_do_sub <= 0;
        addition_ret_to_mul <= 0;
        bool_res <= 0;
        sub_op_1 <= 0;
        sub_op_2 <= 0;
    end

    always @(*) begin
        ramWrite = 0;
        ramAddr = offsetAddr;

        numWrite = 32'hxxxxxxxx;

        case (state)
            LOAD_OP_WRITE: begin
                numWrite = in_num;
                ramWrite = 1;
            end

            ZERO_VARS: begin
                numWrite = 0;
                ramWrite = 1;
            end

            MULTIPLY_WRITE_1: begin
                ramWrite = 1;
                numWrite = mulTemp[31:0];
            end

            MULTIPLY_WRITE_2: begin
                ramWrite = 1;
                numWrite = mulTemp[63:32];
            end

            SHIFT_RES_1: begin
                ramAddr = offsetAddr + in_num[8:0];
            end

            SHIFT_RES_2: begin
                ramWrite = 1;
                numWrite = ((10'd1023 - in_num[8:0]) >= offsetAddr) ? numRead : 0;
            end

            SUBTRACT_4: begin
                ramWrite = 1;
                numWrite = subTemp;
            end

            COPY_RES_LOWER_TO_OP_2_2: begin
                ramWrite = 1;
                numWrite = numRead;
            end

        endcase
    end

    always @(posedge clk) begin
        if (ramWrite)
            RAM1[ramAddr] <= numWrite;
        numRead <= RAM1[ramAddr];
    end

    always @(posedge clk or posedge stb) begin
        if (stb) begin
            state <= INIT;
            done <= 0;
            offsetAddr <= 0;
            arithmetic_do_sub <= 1;
        end else begin
            case (state)
            INIT: begin
                case (operation)
                    3'b000: begin
                        offsetAddr <= addr;
                        state <= LOAD_OP_WRITE;
                    end
                    3'b001: begin
                        offsetAddr <= {2'b10, addr};
                        state <= LOAD_OP_WRITE;
                    end
                    3'b010: begin
                        offsetAddr <= {2'b01, addr};
                        state <= READ_RES;
                    end
                    3'b011: begin
                        state <= ZERO_VARS;
                        mulTemp <= 0;
                        offsetAddr <= 256;
                        addition_ret_to_mul <= 1;
                        arithmetic_do_sub <= 0;
                        sub_op_1 <= 2'b01;
                        sub_op_2 <= 2'b11;
                        offsetAddrMul1 <= 0;
                        offsetAddrMul2 <= 0;
                    end
                    3'b100: begin
                        state <= CMP_1;
                        offsetAddr <= 127; // end of first operand
                    end
                    3'b101: begin
                        state <= SHIFT_RES_1;
                        offsetAddr <= 256;
                    end
                    3'b110: begin
                        state <= SUBTRACT_1;
                        offsetAddr <= 0;
                        subTemp <= 0;
                        carry <= 1;
                        arithmetic_do_sub <= 1;
                        addition_ret_to_mul <= 0;
                        sub_op_1 <= 2'b00;
                        sub_op_2 <= 2'b10;
                    end
                    3'b111: begin
                        offsetAddr <= 256;
                        state <= COPY_RES_LOWER_TO_OP_2_1;
                    end
                endcase
            end

            LOAD_OP_WRITE: begin
                state <= DONE;
            end

            READ_RES: begin
                state <= DONE;
            end

            ZERO_VARS: begin
                /* Only at most 2048 * 3 bit numbers */
                if (&offsetAddr[7:6]) begin
                    if (offsetAddr[9:8] == 2'b01) begin
                        offsetAddr[9:8] <= 2'b11;
                        offsetAddr[7:0] <= 0;
                    end else begin
                        offsetAddr <= {3'b000, offsetAddrMul1};
                        state <= MULTIPLY_START;
                    end
                end
                else begin
                    offsetAddr <= offsetAddr + 1;
                end
            end

            MULTIPLY_START: begin
                mulTemp <= 0;
                offsetAddr <= {3'b100, offsetAddrMul2};
                state <= MULTIPLY_DIGITS_MUL;
            end

            MULTIPLY_DIGITS_MUL: begin
                op1 <= numRead;
                state <= MULTIPLY_DIGITS_MUL_1;
            end

            MULTIPLY_DIGITS_MUL_1: begin
                op2 <= numRead[15:0];
                state <= MULTIPLY_DIGITS_PREPARE;
                digit_multiply_stall <= 1;
            end

            MULTIPLY_DIGITS_PREPARE: begin
                /* Using two separate block RAMs conflicts with
                   routing for multipliers. Thus, I split multiplication
                   of digits into two steps, to leave two multipliers
                    routing empty. */
                if (digit_multiply_stall) begin
                    digit_multiply_stall <= 0;
                    op2 <= numRead[31:16];
                end else begin
                    mulTemp <= mulTemp1;
                    state <= MULTIPLY_DIGITS_PREPARE_1;
                end
                mulTemp1 <= op1 * op2;
            end

            MULTIPLY_DIGITS_PREPARE_1: begin
                offsetAddr[9:8] <= 2'b11;
                offsetAddr[7:0] <= offsetAddrMul1 + offsetAddrMul2;
                mulTemp <= (mulTemp1 << 16) + mulTemp;
                state <= MULTIPLY_WRITE_1;
            end

            MULTIPLY_WRITE_1: begin
                offsetAddr[7:0] <= offsetAddr[7:0] + 1;
                state <= MULTIPLY_WRITE_2;
            end

            MULTIPLY_WRITE_2: begin
                offsetAddr[9:8] <= sub_op_1;
                offsetAddr[7:0] <= 0;
                subTemp <= 0;
                carry <= 0;
                state <= SUBTRACT_1;
                if ((&offsetAddrMul1[6:0]) && (&offsetAddrMul2[6:0])) begin
                    addition_ret_to_mul <= 0;
                end else begin
                    if (&offsetAddrMul2[6:1]) begin
                        if (offsetAddrMul2[0]) begin
                            offsetAddrMul1 <= offsetAddrMul1 + 1;
                            offsetAddrMul2 <= 0;
                        end else begin
                            offsetAddrMul2 <= 1;
                        end
                    end else begin
                        state <= MULTIPLY_START;
                        offsetAddrMul2 <= offsetAddrMul2 + 2;
                        offsetAddr <= {3'b000, offsetAddrMul1};
                    end
                end

                if (in_num[0] && &offsetAddrMul1[5:0] && &offsetAddrMul2[5:0])
                    addition_ret_to_mul <= 0;
            end

            CMP_1: begin
                offsetAddr[9:7] <= 3'b100;
                state <= CMP_2;
            end

            CMP_2: begin
                op1 <= numRead;
                state <= CMP_3;
            end

            CMP_3: begin
                if (op1 == numRead) begin
                    if (offsetAddr[6:0] == 0) begin
                        state <= DONE;
                        bool_res <= 0;
                    end else begin
                        offsetAddr[6:0] <= offsetAddr[6:0] - 1;
                        offsetAddr[9:7] <= 0;
                        state <= CMP_1;
                    end
                end else begin
                    state <= DONE;
                    bool_res <= ((op1 > numRead) ? 1 : 3);
                end
            end

            SHIFT_RES_1: begin
                state <= SHIFT_RES_2;
            end

            SHIFT_RES_2: begin
                if (&offsetAddr[7:0])
                    state <= DONE;
                else begin
                    offsetAddr <= offsetAddr + 1;
                    state <= SHIFT_RES_1;
                end
            end

            SUBTRACT_1: begin
                offsetAddr[9:8] <= sub_op_2;
                state <= SUBTRACT_2;
            end

            SUBTRACT_2: begin
                op1 <= numRead;
                state <= SUBTRACT_3;
            end

            SUBTRACT_3: begin
                offsetAddr[9:8] <= 2'b01;
                {next_carry, subTemp} <= op1 + {1'b0, (arithmetic_do_sub ? (~numRead) : numRead)} + carry;
                state <= SUBTRACT_4;
            end

            SUBTRACT_4: begin
                carry <= next_carry;
                /* Numbers actually operated upon are at most 2048 * 3 bit */
                if ((in_num[0] ? offsetAddr[7] : (&offsetAddr[7:6]))) begin
                    if (addition_ret_to_mul) begin
                        offsetAddr[9:8] <= 2'b11;
                        offsetAddr[7:0] <= 0;
                        state <= ZERO_VARS;
                    end else
                        state <= DONE;
                end
                else begin
                    offsetAddr[7:0] <= offsetAddr[7:0] + 1;
                    offsetAddr[9:8] <= sub_op_1;
                    state <= SUBTRACT_1;
                end
            end

            COPY_RES_LOWER_TO_OP_2_1: begin
                offsetAddr[9:7] <= 3'b100;
                state <= COPY_RES_LOWER_TO_OP_2_2;
            end

            COPY_RES_LOWER_TO_OP_2_2: begin
                if (&offsetAddr[6:0])
                    state <= DONE;
                else begin
                    offsetAddr[6:0] <= offsetAddr[6:0] + 1;
                    offsetAddr[9:7] <= 3'b010;
                    state <= COPY_RES_LOWER_TO_OP_2_1;
                end
            end

            DONE: begin
                out_num <= numRead;
                done <= 1;
            end
            endcase
        end
    end
endmodule


module advanced_coprocessor(
    input wire clk,
    input wire stb,
    input wire [1:0] operation,
    input wire [31:0] in_num,
    input wire [8:0] addr,
    output reg [31:0] out_num,
    output reg done
    );

    localparam IDLE = 0;
    localparam READ_RES = 1;
    localparam LOAD_WRITE = 2;
    localparam READ_RES_DONE = 3;
    localparam POWER_RESULT_TO_1 = 4;
    localparam POWER_FETCH_CHUNK = 5;
    localparam POWER_LOOP = 6;
    localparam DONE = 7;
    localparam POWER_START = 8;
    localparam POWER_LOAD_TO_ACOP = 9;
    localparam POWER_LOAD_TO_ACOP_1 = 10;
    localparam POWER_LOAD_TO_ACOP_2 = 11;
    localparam POWER_LOAD_TO_ACOP_DONE = 12;
    localparam POWER_LOAD_RES_TO_OP_2 = 13;
    localparam POWER_START_SQUARE = 14;
    localparam POWER_SQUARE = 15;
    localparam POWER_LOAD_ACOP_RES_2 = 16;
    localparam POWER_LOAD_ACOP_RES = 17;
    localparam POWER_LOAD_ACOP_RES_1 = 18;
    localparam POWER_MULTIPLY = 19;
    localparam POWER_MULTIPLY_1 = 20;
    localparam POWER_MULTIPLY_2 = 21;
    localparam POWER_MULTIPLY_3 = 22;
    localparam POWER_FINALIZE = 23;
    localparam POWER_FETCH_CHUNK_1 = 24;
    localparam POWER_FETCH_CHUNK_2 = 25;
    localparam POWER_MULTIPLY_4 = 26;
    localparam MODULO_REDUCE_RES_TO_OP_1 = 27;
    localparam MODULO_REDUCE_RESIDUAL_TO_OP_2 = 28;
    localparam MODULO_REDUCE_MULTIPLY_1_START = 29;
    localparam MODULO_REDUCE_MULTIPLY_1_AWAIT = 30;
    localparam MODULO_REDUCE_MULTIPLY_2_START = 31;
    localparam MODULO_REDUCE_MULTIPLY_2_AWAIT = 32;
    localparam MODULO_REDUCE_SHIFT_START = 33;
    localparam MODULO_REDUCE_SHIFT_AWAIT = 34;
    localparam MODULO_REDUCE_ACOP_RES_TO_OP_2_1_START = 35;
    localparam MODULO_REDUCE_ACOP_RES_TO_OP_2_1_AWAIT = 36;
    localparam MODULO_REDUCE_ACOP_RES_TO_OP_2_2_START = 37;
    localparam MODULO_REDUCE_ACOP_RES_TO_OP_2_2_AWAIT = 38;
    localparam MODULO_REDUCE_SUB_1_START = 39;
    localparam MODULO_REDUCE_SUB_1_AWAIT = 40;
    localparam MODULO_REDUCE_SUB_2_START = 41;
    localparam MODULO_REDUCE_SUB_2_AWAIT = 42;
    localparam MODULO_REDUCE_T_TO_OP_1 = 43;
    localparam MODULO_REDUCE_MODULUS_TO_OP_2 = 44;
    localparam MODULO_REDUCE_CMP_START = 45;
    localparam MODULO_REDUCE_CMP_AWAIT = 46;

    reg ramWrite;
    reg [8:0] ramAddr;
    reg [5:0] power_batch_idx;
    reg [31:0] RAM [0:511], numWrite, numRead, powers;
    reg [8:0] offsetAddr, powerAddr;

    reg [5:0] state, acop_load_next_state, acop_read_next_state, modulo_reduce_next_state;

    reg acop_stb;
    reg [2:0] acop_operation;
    reg [31:0] acop_in_num, acop_res;
    reg [7:0] acop_addr;
    wire acop_done;
    wire [31:0] acop_out_num;
    wire [1:0] acop_bool_res;

    reg load_fence, load_fence_all, result_to_one_start;

    initial begin
        state <= IDLE;
        acop_stb <= 1;
        load_fence <= 0;
    end

    arithmetic_coprocesor acop(
        .clk(clk), .stb(acop_stb), .operation(acop_operation),
        .in_num(acop_in_num), .addr(acop_addr), .out_num(acop_out_num),
        .done(acop_done), .bool_res(acop_bool_res));

    always @(*) begin
        ramWrite = 0;
        ramAddr = offsetAddr;
        numWrite = 32'hxxxxxxxx;

        case (state)
        LOAD_WRITE: begin
            numWrite = in_num;
            ramWrite = 1;
        end

        POWER_RESULT_TO_1: begin
            ramWrite = 1;
            numWrite = offsetAddr[6:0] == 0 ? 32'h00000001 : 32'h00000000;
        end

        POWER_LOAD_ACOP_RES_2: begin
            ramWrite = 1;
            numWrite = acop_res;
        end

        endcase
    end

    always @(posedge clk) begin
        if (ramWrite)
            RAM[ramAddr] <= numWrite;
        numRead <= RAM[ramAddr];
    end

    always @(posedge clk or posedge stb) begin
        if (stb) begin
            acop_stb <= 1;
            state <= IDLE;
            done <= 0;
            offsetAddr <= 0;
        end else begin
            case (state)
            IDLE: begin
                case (operation)
                2'b00: begin
                    state <= LOAD_WRITE;
                    offsetAddr <= addr;
                end
                2'b01: begin
                    offsetAddr <= addr;
                    state <= READ_RES;
                end
                2'b10: begin
                    result_to_one_start <= 1;
                    state <= POWER_RESULT_TO_1;
                    offsetAddr <= 256;
                    powerAddr <= 127;
                    power_batch_idx <= 32;
                end
                default: begin
                    state <= DONE;
                end
                endcase
            end

            LOAD_WRITE: begin
                state <= DONE;
            end

            READ_RES: begin
                state <= DONE;
            end

            POWER_RESULT_TO_1: begin
                if (&offsetAddr[6:0]) begin
                    if (result_to_one_start)
                        state <= POWER_START;
                    else
                        state <= POWER_MULTIPLY_2;
                end else
                    offsetAddr <= offsetAddr + 1;
            end

            POWER_START: begin
                acop_operation <= 0;
                state <= POWER_LOAD_TO_ACOP;
                offsetAddr <= 256;
                acop_load_next_state <= POWER_LOAD_RES_TO_OP_2;
            end

            POWER_LOAD_TO_ACOP: begin
                state <= POWER_LOAD_TO_ACOP_1;
            end

            POWER_LOAD_TO_ACOP_1: begin
                acop_in_num <= load_fence_all ? 0 : ((load_fence & offsetAddr[6]) ? 0 : numRead);
                acop_addr <= offsetAddr[6:0];
                acop_stb <= 0;
                state <= POWER_LOAD_TO_ACOP_2;
            end

            POWER_LOAD_TO_ACOP_2: begin
                if (acop_done) begin
                    acop_stb <= 1;

                    if (&offsetAddr[6:0]) begin
                        load_fence <= 0;
                        load_fence_all <= 0;
                        state <= acop_load_next_state;
                    end else begin
                        offsetAddr <= offsetAddr + 1;
                        state <= POWER_LOAD_TO_ACOP;
                    end
                end
            end

            POWER_LOAD_RES_TO_OP_2: begin
                acop_operation <= 1;
                offsetAddr <= 256;
                acop_load_next_state <= POWER_START_SQUARE;
                state <= POWER_LOAD_TO_ACOP;
            end

            POWER_START_SQUARE: begin
                acop_operation <= 3;
                acop_stb <= 0;
                acop_in_num[0] <= 1;
                state <= POWER_SQUARE;
            end

            POWER_SQUARE: begin
                if (acop_done) begin
                    acop_stb <= 1;
                    acop_operation <= 2;
                    acop_addr <= 0;
                    offsetAddr <= 256;
                    state <= POWER_LOAD_ACOP_RES;
                    acop_read_next_state <= MODULO_REDUCE_RES_TO_OP_1;
                    modulo_reduce_next_state <= POWER_FETCH_CHUNK;
                end
            end

            POWER_LOAD_ACOP_RES: begin
                acop_stb <= 0;
                state <= POWER_LOAD_ACOP_RES_1;
            end

            POWER_LOAD_ACOP_RES_1: begin
                if (acop_done) begin
                    acop_stb <= 1;
                    acop_res <= acop_out_num;
                    state <= POWER_LOAD_ACOP_RES_2;
                end
            end

            POWER_LOAD_ACOP_RES_2: begin
                if (&offsetAddr[6:0]) begin
                    state <= acop_read_next_state;
                end else begin
                    acop_addr <= acop_addr + 1;
                    offsetAddr <= offsetAddr + 1;
                    state <= POWER_LOAD_ACOP_RES;
                end
            end

            POWER_FETCH_CHUNK: begin
                if (power_batch_idx[5]) begin
                    power_batch_idx <= 31;
                    offsetAddr <= powerAddr;
                    state <= POWER_FETCH_CHUNK_1;
                end else
                    state <= POWER_MULTIPLY;
            end

            POWER_FETCH_CHUNK_1: begin
                state <= POWER_FETCH_CHUNK_2;
            end

            POWER_FETCH_CHUNK_2: begin
                powers <= numRead;
                state <= POWER_MULTIPLY;
            end

            POWER_MULTIPLY: begin
                acop_load_next_state <= POWER_MULTIPLY_1;
                offsetAddr <= 256;
                acop_operation <= 0;
                state <= POWER_LOAD_TO_ACOP;
            end

            POWER_MULTIPLY_1: begin
                state <= POWER_RESULT_TO_1;
                offsetAddr <= 256;
                result_to_one_start <= 0;
            end

            POWER_MULTIPLY_2: begin
                if (powers[power_batch_idx[4:0]]) begin
                    offsetAddr <= 384;
                end else begin
                    offsetAddr <= 256;
                end
                acop_operation <= 1;
                acop_load_next_state <= POWER_MULTIPLY_3;
                state <= POWER_LOAD_TO_ACOP;
            end

            POWER_MULTIPLY_3: begin
                acop_in_num[0] <= 1;
                acop_operation <= 3;
                acop_stb <= 0;
                state <= POWER_MULTIPLY_4;
            end

            POWER_MULTIPLY_4: begin
                if (acop_done) begin
                    acop_operation <= 2;
                    acop_stb <= 1;
                    acop_addr <= 0;
                    offsetAddr <= 256;
                    acop_read_next_state <= MODULO_REDUCE_RES_TO_OP_1;
                    modulo_reduce_next_state <= POWER_FINALIZE;
                    state <= POWER_LOAD_ACOP_RES;
                end
            end

            POWER_FINALIZE: begin
                if (powerAddr[5:0] == 0 && power_batch_idx == 0)
                    state <= DONE;
                else begin
                    state <= POWER_START;
                    if (power_batch_idx == 0) begin
                        power_batch_idx <= 32;
                        powerAddr <= powerAddr - 1;
                    end
                    else
                        power_batch_idx <= power_batch_idx - 1;
                end
            end

            MODULO_REDUCE_RES_TO_OP_1: begin
                acop_operation <= 0;
                acop_addr <= 0;
                state <= POWER_LOAD_TO_ACOP;
                offsetAddr <= 256;
                acop_load_next_state <= MODULO_REDUCE_RESIDUAL_TO_OP_2;
            end

            MODULO_REDUCE_RESIDUAL_TO_OP_2: begin
                acop_addr <= 0;
                acop_operation <= 1;
                offsetAddr <= 128;
                state <= POWER_LOAD_TO_ACOP;
                acop_load_next_state <= MODULO_REDUCE_MULTIPLY_1_START;
            end

            MODULO_REDUCE_MULTIPLY_1_START: begin
                acop_in_num[0] <= 0;
                acop_operation <= 3;
                acop_stb <= 0;
                state <= MODULO_REDUCE_MULTIPLY_1_AWAIT;
            end

            MODULO_REDUCE_MULTIPLY_1_AWAIT: begin
                if (acop_done) begin
                    acop_stb <= 1;
                    state <= MODULO_REDUCE_SHIFT_START;
                end
            end

            MODULO_REDUCE_SHIFT_START: begin
                acop_operation <= 5;
                acop_in_num <= 128;
                acop_stb <= 0;
                state <= MODULO_REDUCE_SHIFT_AWAIT;
            end

            MODULO_REDUCE_SHIFT_AWAIT: begin
                if (acop_done) begin
                    acop_stb <= 1;
                    state <= MODULO_REDUCE_ACOP_RES_TO_OP_2_1_START;
                end
            end

            MODULO_REDUCE_ACOP_RES_TO_OP_2_1_START: begin
                acop_operation <= 7;
                acop_stb <= 0;
                state <= MODULO_REDUCE_ACOP_RES_TO_OP_2_1_AWAIT;
            end

            MODULO_REDUCE_ACOP_RES_TO_OP_2_1_AWAIT: begin
                if (acop_done) begin
                    // here we go through load of modulus to op_1
                    acop_stb <= 1;
                    state <= POWER_LOAD_TO_ACOP;
                    load_fence <= 1;
                    offsetAddr <= 0;
                    acop_operation <= 0;
                    acop_load_next_state <= MODULO_REDUCE_MULTIPLY_2_START;
                end
            end

            MODULO_REDUCE_MULTIPLY_2_START: begin
                acop_in_num[0] <= 1;
                acop_operation <= 3;
                acop_stb <= 0;
                state <= MODULO_REDUCE_MULTIPLY_2_AWAIT;
            end

            MODULO_REDUCE_MULTIPLY_2_AWAIT: begin
                if (acop_done) begin
                    acop_stb <= 1;
                    state <= MODULO_REDUCE_ACOP_RES_TO_OP_2_2_START;
                end
            end

            MODULO_REDUCE_ACOP_RES_TO_OP_2_2_START: begin
                acop_operation <= 7;
                acop_stb <= 0;
                state <= MODULO_REDUCE_ACOP_RES_TO_OP_2_2_AWAIT;
            end

            MODULO_REDUCE_ACOP_RES_TO_OP_2_2_AWAIT: begin
                if (acop_done) begin
                    acop_stb <= 1;
                    state <= POWER_LOAD_TO_ACOP;
                    offsetAddr <= 256;
                    acop_operation <= 0;
                    acop_addr <= 0;
                    acop_load_next_state <= MODULO_REDUCE_SUB_1_START;
                end
            end

            MODULO_REDUCE_SUB_1_START: begin
                acop_operation <= 6;
                acop_stb <= 0;
                state <= MODULO_REDUCE_SUB_1_AWAIT;
            end

            MODULO_REDUCE_SUB_1_AWAIT: begin
                if (acop_done) begin
                    acop_operation <= 2;
                    acop_addr <= 0;
                    offsetAddr <= 256;
                    state <= POWER_LOAD_ACOP_RES;
                    acop_read_next_state <= MODULO_REDUCE_T_TO_OP_1;
                    acop_stb <= 1;
                end
            end

            MODULO_REDUCE_T_TO_OP_1: begin
                acop_operation <= 0;
                acop_addr <= 0;
                state <= POWER_LOAD_TO_ACOP;
                offsetAddr <= 256;
                acop_load_next_state <= MODULO_REDUCE_MODULUS_TO_OP_2;
            end

            MODULO_REDUCE_MODULUS_TO_OP_2: begin
                state <= POWER_LOAD_TO_ACOP;
                load_fence <= 1;
                offsetAddr <= 0;
                acop_operation <= 1;
                acop_load_next_state <= MODULO_REDUCE_CMP_START;
            end

            MODULO_REDUCE_CMP_START: begin
                acop_operation <= 4;
                acop_stb <= 0;
                state <= MODULO_REDUCE_CMP_AWAIT;
            end

            MODULO_REDUCE_CMP_AWAIT: begin
                if (acop_done) begin
                    acop_stb <= 1;

                    if (acop_bool_res == 3) // t < n
                        load_fence_all <= 1;
                    else
                        load_fence <= 1;
                    offsetAddr <= 0;
                    acop_operation <= 1;
                    acop_load_next_state <= MODULO_REDUCE_SUB_2_START;
                    state <= POWER_LOAD_TO_ACOP;
                end
            end

            MODULO_REDUCE_SUB_2_START: begin
                acop_operation <= 6;
                acop_stb <= 0;
                state <= MODULO_REDUCE_SUB_2_AWAIT;
            end

            MODULO_REDUCE_SUB_2_AWAIT: begin
                if (acop_done) begin
                    acop_operation <= 2;
                    acop_addr <= 0;
                    offsetAddr <= 256;
                    state <= POWER_LOAD_ACOP_RES;
                    acop_read_next_state <= modulo_reduce_next_state;
                    acop_stb <= 1;
                end
            end

            DONE: begin
                out_num <= numRead;
                done <= 1;
            end
            endcase
        end
    end
endmodule


module crypto (
    input wire CLK,

    input wire EPP_ASTB,
    input wire EPP_DSTB,
    inout wire [7:0] EPP_D,
    input wire EPP_WR,
    output reg EPP_WAIT,
    output reg [7:0] led
    );

    reg [1:0] epp_state;
    reg astb, dstb, wr, rdata_oe;
    reg [7:0] wdata, rdata;
    wire [10:0] epp_input, epp_synced;

    localparam EPP_INIT = 0;
    localparam EPP_WAIT_ST = 1;
    localparam EPP_AWAIT_OPERATION_DONE = 2;
    localparam EPP_AWAIT_ACOP_OPERATION_DONE = 3;

    reg [2:0] crypto_register;

    localparam ADDR_RESET = 0;
    localparam ADDR_PRIVATE_KEY_LOAD = 1;
    localparam ADDR_MESSAGE = 2;
    localparam ADDR_RES = 3;
    localparam ADDR_ENCRYPT = 4;
    localparam ADDR_STATUS = 5;
    localparam ADDR_RNG = 6;
    localparam ADDR_DIAGNOSTIC = 7;

    reg adv_stb;
    reg [1:0] adv_operation;
    reg [31:0] adv_in_num;
    reg [8:0] adv_addr;
    wire adv_done, adv_operation_in_progress;

    wire [31:0] adv_out_num;

    reg [7:0] buffer[0:2];
    reg [7:0] offsetRes;
    reg [7:0] offsetMsg;
    reg [9:0] offsetPrivKey;

    wire [7:0] rng_random;
    wire clk_;

    RNG rng(
        .clk(CLK),
        .reset(0),
        .out(rng_random));

    integer i;
    initial begin
        epp_state <= EPP_INIT;
        wdata <= 0;
        astb <= 1;
        dstb <= 1;
        rdata_oe <= 0;
        crypto_register <= ADDR_RESET;

        adv_addr <= 0;
        adv_stb <= 1;

        offsetPrivKey <= 0;
        offsetMsg <= 0;
        offsetRes <= 0;
        for (i=0; i<3; i = i + 1)
          begin
            buffer[i] <= 0;
          end
        led <= 0;
    end

    assign epp_input[0] = EPP_ASTB;
    assign epp_input[1] = EPP_DSTB;
    assign epp_input[9:2] = EPP_D;
    assign epp_input[10] = EPP_WR;
    assign EPP_D = rdata_oe ? rdata : 8'hzz;

    assign adv_operation_in_progress = !adv_stb && !adv_done;

    DCM_SP #(
        .CLKFX_DIVIDE(5),
        .CLKFX_MULTIPLY(8)
    ) display_dcm (
        .CLKIN(CLK),
        .CLKFX(clk_),
        .RST(0)
    );

    //assign clk_ = CLK;

    synchronize_inputs #(.BITS(11), .SYNC_LEVEL(3)) sync(.inputs(epp_input), .clk(clk_), .sync_inputs(epp_synced));

    advanced_coprocessor adv_cop(.clk(clk_), .stb(adv_stb), .operation(adv_operation),
        .in_num(adv_in_num), .addr(adv_addr), .out_num(adv_out_num),
        .done(adv_done));

    // EPP
    always @(posedge clk_) begin
        astb <= epp_synced[0];
        dstb <= epp_synced[1];
        wdata <= epp_synced[9:2];
        wr <= epp_synced[10];

        if (!adv_operation_in_progress) begin
            adv_stb <= 1;
            led <= 0;
        end else
            led <= rng_random;

        case (epp_state)
        EPP_INIT: begin
            EPP_WAIT <= 0;

            if (!astb && !wr) begin
                if (!(|wdata[7:3])) begin
                    crypto_register <= wdata;
                end
                epp_state <= EPP_WAIT_ST;
            end

            if (!astb && wr) begin
                epp_state <= EPP_WAIT_ST;
                rdata <= crypto_register;
            end

            if (!dstb && !wr) begin
                if (!adv_operation_in_progress) begin
                    epp_state <= EPP_AWAIT_OPERATION_DONE;
                    case (crypto_register)
                    ADDR_RESET: begin
                        offsetMsg <= 0;
                        offsetPrivKey <= 0;
                        offsetRes <= 0;
                        epp_state <= EPP_WAIT_ST;
                        for (i=0; i<3; i = i + 1)
                          begin
                            buffer[i] <= 0;
                          end
                    end

                    ADDR_PRIVATE_KEY_LOAD: begin
                        offsetPrivKey <= offsetPrivKey + 1;
                        if (&offsetPrivKey[1:0]) begin
                            adv_addr <= offsetPrivKey[9:2];
                            adv_stb <= 0;
                            adv_operation <= 0;
                            adv_in_num <= {wdata, buffer[2], buffer[1], buffer[0]};
                        end else begin
                            buffer[offsetPrivKey] <= wdata;
                            epp_state <= EPP_WAIT_ST;
                        end
                    end

                    ADDR_MESSAGE: begin
                        offsetMsg <= offsetMsg + 1;
                        if (&offsetMsg[1:0]) begin
                            adv_addr <= {3'b110, offsetMsg[7:2]};
                            adv_stb <= 0;
                            adv_operation <= 0;
                            adv_in_num <= {wdata, buffer[2], buffer[1], buffer[0]};
                        end else begin
                            buffer[offsetMsg] <= wdata;
                            epp_state <= EPP_WAIT_ST;
                        end
                    end

                    ADDR_ENCRYPT: begin
                        adv_stb <= 0;
                        adv_operation <= 2;
                        epp_state <= EPP_WAIT_ST;
                    end

                    ADDR_DIAGNOSTIC: begin
                        led <= !led;
                        epp_state <= EPP_WAIT_ST;
                    end
                    endcase
                end else
                    epp_state <= EPP_INIT;
            end

            if (!dstb && wr) begin
                rdata_oe <= 1;
                epp_state <= EPP_WAIT_ST;

                case (crypto_register)
                ADDR_RES: begin
                    if (!adv_operation_in_progress) begin
                        offsetRes <= offsetRes + 1;
                        adv_addr <= {3'b100, offsetRes[7:2]};
                        adv_stb <= 0;
                        adv_operation <= 1;
                        epp_state <= EPP_AWAIT_OPERATION_DONE;
                    end
                end

                ADDR_STATUS: begin
                    rdata <= adv_operation_in_progress;
                    epp_state <= EPP_WAIT_ST;
                end

                ADDR_RNG: begin
                    rdata <= rng_random;
                    epp_state <= EPP_WAIT_ST;
                end
                endcase
            end
        end

        EPP_AWAIT_OPERATION_DONE: begin
            if (adv_done) begin
                case (offsetRes[1:0])
                2'b00:
                    rdata <= adv_out_num[31:24];
                2'b01:
                    rdata <= adv_out_num[7:0];
                2'b10:
                    rdata <= adv_out_num[15:8];
                2'b11:
                    rdata <= adv_out_num[23:16];
                endcase
                adv_stb <= 1;
                epp_state <= EPP_WAIT_ST;
            end
        end

        EPP_WAIT_ST: begin
            EPP_WAIT <= 1;
            if (astb && dstb) begin
                rdata_oe <= 0;
                epp_state <= EPP_INIT;
            end
        end
        endcase
    end
endmodule
