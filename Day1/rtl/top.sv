module top(input logic [15:0]  data_i,
           input logic         valid_i,
           input logic         clk,
           input logic         rst_n,
           output logic        ready_o,
           output logic valid_o,
           output logic [31:0] password_o);

    logic [15:0] dividend;
    logic [15:0] quotient;
    logic [15:0] remainder;

    logic        init;
    logic        stop;

    logic [31:0] turns;

    logic        ready1;
    logic        valid2, ready2;
    logic        valid3;

    logic [15:0] cnt;
    logic [15:0] total_num_turns;

    typedef enum logic[2:0] {
        IDLE,
        CHECK_TOTAL_CNT,
        ACTIVE,
        WAIT1,
        WAIT2,
        DONE
    }state_t;

    state_t state;

    always_ff @(posedge clk) begin
        if(!rst_n) begin
            state <= IDLE;
            valid_o <= 0;
            init <= 1;
            stop <= 1;
            cnt <= 0;
        end
        else begin
            unique case(state)
                IDLE: begin
                    if(valid_i && ready_o) begin
                        init <= 0;
                        state <= CHECK_TOTAL_CNT;
                    end
                end
                CHECK_TOTAL_CNT: begin
                    if(cnt == total_num_turns) begin
                        state <= DONE;
                    end
                    else begin
                        state <= ACTIVE;
                        stop <= 0;
                        init <= 0;
                    end
                end
                ACTIVE:begin
                    if(ready_o && valid_i) begin
                        cnt <= cnt + 1;

                        if(cnt + 1 == total_num_turns) begin
                            state <= WAIT1;
                            stop <= 1;
                        end
                    end
                end
                WAIT1: begin
                    state <= WAIT2;
                end
                WAIT2: begin
                    valid_o <= 1;
                    state <= DONE;
                end
                DONE:begin
                    valid_o <= 0;
                    stop <= 1;
                    init <= 1;
                    state <= IDLE;
                end
            endcase
        end
    end

    input_circuit ic (
        .clk (clk),
        .rst_n (rst_n),
        .data_i (data_i),
        .valid_1 (valid_i),
        .ready_1 (ready_o),
        .data_o (dividend),
        .valid_2 (valid_2),
        .ready_2 (ready_2),
        .total_num_turns_o (total_num_turns),
        .init_i (init),
        .stop_i (stop));

    divider d(.dividend (dividend),
              .quotient (quotient),
              .remainder (remainder));

    dial_solver ds(.val (remainder),
                  .valid_i (valid_2),
                  .clk (clk),
                  .rst_n (rst_n),
                  .ready (ready_2),
                  .valid (valid_3),
                  .password (password_o),
                  .full_turns (quotient));

endmodule
