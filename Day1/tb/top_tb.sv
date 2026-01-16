module top_tb;

    logic clk;
    logic rst_n;

    logic ready_i;
    logic valid_i;

    logic valid_o;

    logic [15:0] val;
    logic [31:0] password;

    logic [31:0] expected_dial = 50;
    logic [31:0] expected_password = 0;

    top d(.data_i (val),
                  .valid_i (valid_i),
                  .clk (clk),
                  .rst_n (rst_n),
                  .ready_o (ready_i),
                  .valid_o (valid_o),
                  .password_o (password));

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

    @(negedge clk) begin
        val <= i_val;
        valid_i <= 1;
    end

    wait(ready_i);

    @(posedge clk) begin
        assert(valid_i && ready_i);
        $display("Transfer of type:%b val: %lu completed. \n", val[15], val[14:0]);
    end
    @(negedge clk)
        valid_i <= 0;
endtask

    function automatic int load_test(input string filename);
        int fd = $fopen (filename, "r");
        if (fd)  $display("File was opened successfully : %0d", fd);
        else     $display("File was NOT opened successfully : %0d", fd);

        return fd;
    endfunction

function automatic int count_file_lines(input int fd);
    int line_count = 0;
    string line;

    while ($fgets(line, fd)) begin
        line_count++;
    end

    void'($fseek(fd, 0, 0));

    if ($ftell(fd) != 0) begin
        $warning("File pointer did not reset correctly.");
    end

    return line_count;
endfunction

    initial gen_clock(100, clk);

    string file_path;
    int fd;

    string line;
    string num_part;
    int    num_lines;

    initial begin
        // reset
        @(posedge clk)
          rst_n <= 0;
        @(posedge clk)
          rst_n <= 1;

        // Get file path from TCL via +FILE_PATH argument
        if (!$value$plusargs("FILE_PATH=%s", file_path)) begin
            $display("No file path provided via +FILE_PATH");
            $finish;
        end else begin
            $display("File path from command line: %s", file_path);
        end

        // Open and use the file
        fd = load_test(file_path);


        num_lines = count_file_lines(fd);

        send_val(num_lines);

        while (!$feof(fd)) begin
            $fgets(line, fd);
            num_part = line.substr(1, line.len() - 1);
            val = num_part.atoi();
            $display ("Line: %s", line);
            $display ("val: %d", val);
            val[15] = (line[0] == "R") ? 1 : 0;

            send_val(val);
            tick_expected_dial(val);
        end

        // TODO debug wait
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);
        @(posedge clk);

        @(posedge clk)
            $display("Password %d \n", password);

        assert (expected_password == password)
        else $error("Difference in password got %lu expected %lu \n", password, expected_password);

        $finish;
    end

endmodule
