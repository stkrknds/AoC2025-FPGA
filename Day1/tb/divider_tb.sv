module divider_tb;

logic [31:0] dividend;
logic [31:0] quotient;
logic [31:0] remainder;

    divider d(.dividend (dividend),
    .quotient (quotient),
              .remainder (remainder));


    initial begin
        dividend = 150;
        #5ns;

        $display("quotient %d remainder %d \n", quotient, remainder);
    end

endmodule
