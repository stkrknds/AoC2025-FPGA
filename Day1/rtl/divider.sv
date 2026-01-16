module divider(
    input logic [15:0]  dividend,
    output logic [15:0] quotient,
    output logic [15:0] remainder
);

    // M = 18351 (0x000047AF)
    // S = 6

    localparam logic[15:0] MAGIC = 16'h000047AF;

    logic [31:0]           temp;
    logic [31:0]           q;
    logic [15:0]           diff, t;

    assign temp = MAGIC * dividend[14:0];

    assign q = temp[31:16];

    assign diff = (dividend[14:0] - q) >> 1;

    assign t = diff + q;

    assign quotient = t >> 6;

    // multiply by 10 with shifts
    assign remainder[14:0] = dividend[14:0] - ((quotient << 6) + (quotient << 5) + (quotient << 2));

    // pass the type of the turn to the output
    assign remainder[15] = dividend[15];
endmodule
