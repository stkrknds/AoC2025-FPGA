module dial_solver_tb;

    logic clk;
    logic ready_i;
    logic valid_o;
    logic rst_n;

    logic [15:0] val;
    logic [31:0] password;

    logic [31:0] expected_dial = 50;
    logic [31:0] expected_password = 0;
    logic [15:0] full_turns = 0;

    dial_solver d(.val (val),
                  .valid_i (valid_o),
                  .clk (clk),
                  .rst_n (rst_n),
                  .full_turns (full_turns),
                  .ready (ready_i),
                  .valid (valid_o),
                  .password (password));

function automatic logic[15:0] gen_dial_move();
    logic turn_type = $urandom;

    logic[11:0] turns = $urandom_range(0, 100);

    return {turn_type, 3'b000, turns};
endfunction

task automatic gen_clock(input real freq, ref logic clock);
    real clk_half_period;

    clk_half_period = 2 / freq;
    clock = 0;

    forever #(clk_half_period) clock = ~clock;
endtask

function automatic tick_expected_dial(input logic[15:0] val);
    while(val[14:0] != 0) begin
        if(val[15])
            expected_dial = (expected_dial + 1)%100;
        else begin
            if(expected_dial == 0)
              expected_dial = 99;
            else
               expected_dial = expected_dial - 1;
        end

        val[14:0] = val[14:0] - 1;

        if(expected_dial == 0)
          expected_password = expected_password + 1;
    end
endfunction

task automatic send_val(input logic[15:0] i_val);
    real clk_half_period;

    @(posedge clk) begin
        val <= i_val;
        valid_o <= 1;
    end

    wait(ready_i);

    @(posedge clk) begin
        assert(valid_o && ready_i);
        $display("Transfer of type:%b val: %lu completed. \n", val[15], val[14:0]);
        valid_o <= 0;
    end
endtask

    initial gen_clock(100, clk);

    initial begin
        // reset
        @(posedge clk)
          rst_n <= 0;
        @(posedge clk)
          rst_n <= 1;

        repeat(10) begin
            send_val(gen_dial_move());
            tick_expected_dial(val);
        end

        assert (expected_password == password)
        else $error("Difference in pass got %lu expected %lu \n", password, expected_password);

        @(posedge clk)
            $display("Password %d \n", password);

        $finish;
    end

endmodule
