module dial_solver(
        input logic [15:0] val,
        input logic        valid_i,
        input logic        clk,
        input logic        rst_n,
        input logic [15:0] full_turns,
        output logic       ready,
        output logic       valid,
        output logic [31:0] password
);

    logic signed[31:0] dial;
    logic signed[31:0] old_dial;
    logic [15:0] rval;

    typedef enum logic {
        LEFT = 0,
        RIGHT = 1
    } turn_t;

    typedef enum logic[2:0]{
        IDLE,
        READY,
        CHECK_TURN,
        R_END,
        L_END
    }state_t;

    logic turn;
    state_t state;

    assign turn = val[15];

    always_ff @(posedge clk) begin
        if(!rst_n) begin
            ready <= 0;
            password <= 0;
            dial <= 50;
            state <= IDLE;
        end
        else begin
            unique case(state)
                IDLE: begin
                    ready <= 1;
                    state <= READY;
                end
                READY: begin
                    if(valid_i) begin
                      // keep a copy of the dial
                      old_dial <= dial;
                      password <= password + full_turns;
                      rval <= val;
                      state <= CHECK_TURN;
                      ready <= 0;
                    end
                end
                CHECK_TURN: begin
                    if(turn == RIGHT) begin
                      dial <= dial + rval[14:0];
                      state <= R_END;
                    end
                    else begin
                      dial <= dial - rval[14:0];
                      state <= L_END;
                    end
                end
                R_END: begin
                    if(dial > 99) begin
                        dial <= dial - 100;
                        password <= password + 1;
                    end
                    state <= READY;
                    ready <= 1;
                end
                L_END: begin
                    if(dial < 0) begin
                      dial <= dial + 100;
                    end

                    if((dial < 0 || dial == 0) && old_dial != 0)
                        password <= password + 1;

                    state <= READY;
                    ready <= 1;
                end
            endcase
        end
    end

endmodule
