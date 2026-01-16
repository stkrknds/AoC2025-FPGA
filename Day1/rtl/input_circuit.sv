module input_circuit(
    input logic clk,
    input logic rst_n,
    input logic init_i,
    input logic stop_i,

    // left side
    input logic [15:0] data_i,
    input logic valid_1,
    output logic ready_1,

    // right side
    output logic [15:0] data_o,
    output logic valid_2,
    input  logic ready_2,

    output logic[15:0] total_num_turns_o
);

    logic[15:0] skid_buffer;
    logic       skid_buffer_empty;
    logic       r_init;

    logic       t1_done, t2_done;

    // when ready & valid a transfer takes place
    assign t1_done = ready_1 && valid_1;
    assign t2_done = ready_2 && valid_2;

    always_ff @(posedge clk) begin
        if(!rst_n) begin
            ready_1 <= 0;
        end
        else begin
            if(r_init)
              ready_1 <= 1;
            else begin
                if(!stop_i) begin
                    if(skid_buffer_empty) begin
                        // if we have no valid data or a transfer takes place
                        // we can accept new data
                        if((!valid_2) || t2_done)
                          ready_1 <= 1;
                        else ready_1 <= 0;
                    end else ready_1 <= 0;
                end
                else
                    ready_1 <= 0;
            end
        end
    end

    always_ff @(posedge clk) begin
        if(!rst_n) begin
            valid_2 <= 0;
        end
        else begin
            if(r_init)
              valid_2 <= 0;
            else begin
                if(t2_done) begin
                    if(skid_buffer_empty) begin
                        if(t1_done) begin
                            data_o <= data_i;
                            valid_2 <= 1;
                        end
                        else begin
                            valid_2 <= 0;
                        end
                    end
                    else begin
                        data_o <= skid_buffer;
                        valid_2 <= 1;
                    end
                end
                else if(!valid_2) begin
                    if(t1_done) begin
                        data_o <= data_i;
                        valid_2 <= 1;
                    end
                end
            end
        end
    end

    // skid buffer state logic
    always_ff @(posedge clk) begin
        if(!rst_n) begin
            r_init <= 0;
            skid_buffer_empty <= 1;
        end
        else begin
            if(r_init) begin
                if(t1_done) begin
                    r_init <= 0;
                    total_num_turns_o <= data_i;
                end
            end
            else begin
                if(init_i)
                    r_init <= 1;

                if(t1_done && valid_2 && !t2_done) begin
                    skid_buffer <= data_i;
                    skid_buffer_empty <= 0;
                end
                else if(!t1_done && t2_done && !skid_buffer_empty) begin
                    skid_buffer_empty <= 1;
                end
            end
        end
    end

endmodule
