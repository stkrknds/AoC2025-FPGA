module type Config = sig
  val num_rows : int
  val num_cols : int
  val m : int
end

module Make (C : Config) = struct
  open! Core
  open! Hardcaml
  open! Hardcaml.Ram
  open! Signal

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; valid : 'a
      ; data_in : 'a [@bits C.m]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ready : 'a
      ; ddone : 'a
      ; result : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | FirstRow
      | LPad
      | Active
      | RPad
      | LastRow
      | Wait
      | Check
      | BFirstRow
      | BLPad
      | BActive
      | BRPad
      | BLastRow
      | BWait
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create_1r1w_memory ~clock ~wr_addr ~wr_data ~wr_en ~rd_addr =
    let depth = 1 lsl width wr_addr in
    let write_port =
      { Write_port.write_clock = clock
      ; write_address = wr_addr
      ; write_enable = wr_en
      ; write_data = wr_data
      }
    in
    let read_port =
      { Read_port.read_clock = clock; read_address = rd_addr; read_enable = Signal.vdd }
    in
    let q =
      Ram.create
        ~collision_mode:Read_before_write
        ~size:depth
        ~write_ports:[| write_port |]
        ~read_ports:[| read_port |]
        ()
    in
    q.(0)
  ;;

  (* Routes inputs to the last row of the window. *)
  (* When m > 1, some data must be buffered to account for padding. *)
  let router inserting_pad_row spec col_cnt input =
    if C.m = 1
    then [ mux2 (col_cnt ==:. 0 |: (col_cnt >=:. C.num_cols + 1)) gnd input ]
    else (
      let split_input = split_lsb ~part_width:1 input |> Array.of_list in
      let delay_buffer = Signal.reg spec split_input.(0) in
      let out_list =
        List.init (C.m - 1) ~f:(fun bit_idx ->
          (* Insert left/right zero-padding when processing the first or last column. *)
          mux2
            (col_cnt +:. (bit_idx + 1)
             ==:. 0
             |: (col_cnt +:. 1 +:. (bit_idx + 1) >=:. C.num_cols + 1)
             |: inserting_pad_row)
            gnd
            split_input.(bit_idx + 1))
      in
      let reg_out =
        mux2 (col_cnt ==:. 0 |: (col_cnt >=:. C.num_cols + 1)) gnd delay_buffer
      in
      let output = out_list @ [ reg_out ] in
      output)
  ;;

  (* Not all window outputs are valid per cycle. When valid < m, *)
  (* we must delay writing to properly fill the m-width buffer. *)
  let buffer_writer spec win_outputs win_valids =
    let open Always in
    let buffer_size = 2 * C.m in
    let buffer = Array.init buffer_size ~f:(fun _ -> Variable.reg spec ~width:1) in
    let next_stored_cnt =
      Array.init (C.m + 1) ~f:(fun _ -> Variable.wire ~default:(zero 8) ())
    in
    let stored_cnt = Signal.reg spec next_stored_cnt.(C.m).value in
    let wr_addr = Variable.reg spec ~width:16 in
    let wr_en = Always.Variable.wire ~default:(Signal.zero 1) () in
    let max_addr = (C.num_rows * C.num_cols / C.m) - 1 in
    compile
      [ (* when we have m valid outputs we can write *)
        wr_en <-- (stored_cnt >=:. C.m)
      ; (* if a write operation takes place in this cycle then m elements are removed from the buffer *)
        if_
          wr_en.value
          [ next_stored_cnt.(0) <-- stored_cnt -:. C.m
          ; if_
              (wr_addr.value ==:. max_addr)
              [ wr_addr <--. 0 ]
              [ wr_addr <-- wr_addr.value +:. 1 ]
          ; (* when a write operation happens, the stored ouputs *)
            (* from the last m registers(indices m .. buffer_size - 1) *)
            (* are transferred to the first m registers *)
            (* (indices 0 .. m - 1) *)
            proc (List.init C.m ~f:(fun j -> buffer.(j) <-- buffer.(C.m + j).value))
          ]
          [ proc (List.init buffer_size ~f:(fun j -> buffer.(j) <-- buffer.(j).value))
          ; next_stored_cnt.(0) <-- stored_cnt
          ]
      ; (* TODO: Some heavy logic, here can be done better *)
        proc
          (List.init C.m ~f:(fun i ->
             proc
               [ (* by default just pass the signal *)
                 next_stored_cnt.(i + 1) <-- next_stored_cnt.(i).value
               ; when_
                   win_valids.(i)
                   [ proc
                       (List.init buffer_size ~f:(fun j ->
                          when_
                            (next_stored_cnt.(i).value ==:. j)
                            [ buffer.(j) <-- win_outputs.(i) ]))
                   ; next_stored_cnt.(i + 1) <-- next_stored_cnt.(i).value +:. 1
                   ]
               ]))
      ];
    (* the first m registers are written in memory *)
    let wr_data =
      Array.sub buffer ~pos:0 ~len:C.m
      |> Array.map ~f:(fun v -> v.value)
      |> Array.to_list
      |> concat_msb
    in
    wr_addr.value, wr_en.value, wr_data
  ;;

  let create scope (i : Signal.t I.t) : Signal.t O.t =
    let open Always in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) spec in
    (* the first and last rows are all zeros *)
    let inserting_pad_row = sm.is LastRow |: sm.is FirstRow in
    (* Number of rolls that have been removed *)
    let%hw_var remove_cnt = Variable.reg spec ~width:16 in
    let ready_o = Variable.reg spec ~width:1 in
    let done_o = Variable.reg spec ~width:1 in
    let result = Variable.reg spec ~width:64 in
    let%hw_var row_cnt = Variable.reg spec ~width:8 in
    let%hw_var next_col_cnt = Variable.wire ~default:(zero 8) () in
    let col_cnt = Signal.reg spec next_col_cnt.value in
    (* Once the right edge of the window reaches or exceeds the last column, *)
    (* processing for the current row is complete. *)
    let is_end_row = next_col_cnt.value +:. (C.m - 1) >=:. C.num_cols + 1 in
    let is_last_row = row_cnt.value ==:. C.num_rows in
    let%hw_var win_en = Variable.wire ~default:(zero 1) () in
    let delay_win_en = Signal.reg spec win_en.value in
    (* Each time the window is enabled(values shifted in) we need to store them in the linebuffer *)
    (* so win_en acts as a wr_en *)
    let router_inputs = Signal.wire C.m in
    let win_input = router inserting_pad_row spec col_cnt router_inputs in
    (* Instantiate 2 linebuffers *)
    let linebuffer2_out =
      create_1r1w_memory
        ~clock:i.clock
        ~wr_addr:col_cnt
        ~wr_data:(concat_lsb win_input)
        ~wr_en:win_en.value
        ~rd_addr:next_col_cnt.value
    in
    let linebuffer1_out =
      create_1r1w_memory
        ~clock:i.clock
        ~wr_addr:col_cnt
        ~wr_data:linebuffer2_out
        ~wr_en:win_en.value
        ~rd_addr:next_col_cnt.value
    in
    (* TODO refactor inputs *)
    (* Window *)
    (* This register matrix represents the current sliding window. *)
    let window win_en lb1_in lb2_in rinput spec =
      let rows = 3 in
      let cols = 2 + C.m in
      let split_lb1 = split_msb ~part_width:1 lb1_in |> Array.of_list in
      let split_lb2 = split_msb ~part_width:1 lb2_in |> Array.of_list in
      (* Split the inputs and distribute them across the registers of the last m columns. *)
      let reg_matrix =
        Array.init rows ~f:(fun _ ->
          Array.init cols ~f:(fun _ -> Variable.reg spec ~width:1 ~enable:win_en))
      in
      compile
        [ proc
            (List.init rows ~f:(fun r ->
               proc
                 (List.init cols ~f:(fun c ->
                    (* TODO refactor *)
                    if c >= 2
                    then (
                      let sidx = c - 2 in
                      if r = 0
                      then reg_matrix.(r).(c) <-- split_lb1.(sidx)
                      else if r = 1
                      then reg_matrix.(r).(c) <-- split_lb2.(sidx)
                      else reg_matrix.(r).(c) <-- rinput.(C.m - 1 - sidx))
                    else reg_matrix.(r).(c) <-- reg_matrix.(r).(c + C.m).value))))
        ];
      reg_matrix
    in
    let win =
      window win_en.value linebuffer1_out linebuffer2_out (Array.of_list win_input) spec
    in
    (* connect window to get output *)
    let results =
      Array.init C.m ~f:(fun i ->
        let bits =
          concat_lsb
            [ win.(0).(i + 0).value
            ; win.(0).(i + 1).value
            ; win.(0).(i + 2).value
            ; win.(1).(i + 0).value
            ; win.(1).(i + 2).value
            ; win.(2).(i + 0).value
            ; win.(2).(i + 1).value
            ; win.(2).(i + 2).value
            ]
        in
        let count = popcount bits in
        let cur_val = win.(1).(i + 1).value in
        (* can only remove if there are fewer than 4 occupied points and there is a there *)
        let to_remove = count <:. 4 &: cur_val in
        (* when we cannot remove we just pass the current value to the output *)
        let output = mux2 to_remove gnd win.(1).(i + 1).value in
        output, to_remove)
    in
    let outputs = Array.map results ~f:fst in
    let to_removes = Array.map results ~f:snd in
    let is_inside_horizontally = row_cnt.value >=:. 2 in
    let is_inside_vertically =
      Array.init C.m ~f:(fun bit_idx ->
        let cur_col = col_cnt +:. bit_idx in
        let min_col = 2 in
        (* we have to take padding into account so the max index is (num_cols - 1) + 2(pads) *)
        let max_col = C.num_cols + 1 in
        cur_col >=:. min_col &: (cur_col <=:. max_col))
    in
    let win_valid =
      Array.init C.m ~f:(fun idx ->
        Signal.reg spec (is_inside_horizontally &: is_inside_vertically.(idx)))
    in
    let%hw_var next_br_addr = Variable.wire ~default:(zero 16) () in
    let br_addr = Signal.reg spec next_br_addr.value in
    let bwr_addr, bwr_en, bwr_data = buffer_writer spec outputs win_valid in
    (* instatiate buffer *)
    let buffer =
      create_1r1w_memory
        ~clock:i.clock
        ~wr_addr:bwr_addr
        ~wr_data:bwr_data
        ~wr_en:bwr_en
        ~rd_addr:next_br_addr.value
    in
    (* We only count valid removes *)
    let masked_removes = Array.mapi to_removes ~f:(fun i bit -> bit &: win_valid.(i)) in
    let remove_num = popcount (concat_lsb (Array.to_list masked_removes)) in
    (* Asserted if at least one element was removed during this cycle. *)
    let%hw_var was_removed = Variable.reg spec ~width:1 in
    (* Switch the data source from i.data_in to the buffer output once buffer reading begins. *)
    let%hw_var switch_inputs = Variable.reg spec ~width:1 in
    let rrinputs = mux2 switch_inputs.value buffer i.data_in in
    Signal.(router_inputs <-- rrinputs);
    compile
      [ if_
          (sm.is Check)
          [ was_removed <--. 0 ]
          [ when_ (delay_win_en &: (remove_num >:. 0)) [ was_removed <--. 1 ] ]
      ; when_
          delay_win_en
          [ remove_cnt
            <-- remove_cnt.value +: uresize remove_num ~width:(width remove_cnt.value)
          ]
      ; result <--. 0
      ; (* buffer signals *)
        (* we only read from the buffer when we are at the following states *)
        sm.switch
          [ ( BLPad
            , [ (if C.m > 1
                 then next_br_addr <-- br_addr +:. 1
                 else next_br_addr <-- br_addr)
              ] )
          ; BActive, [ next_br_addr <-- br_addr +:. 1 ]
          ; Check, [ next_br_addr <--. 0 ]
          ]
          ~default:[ next_br_addr <-- br_addr ]
      ; sm.switch
          [ Idle, []
          ; FirstRow, [ win_en <--. 1 ]
          ; LPad, [ (if C.m = 1 then win_en <--. 1 else win_en <-- i.valid) ]
          ; Active, [ win_en <-- i.valid ]
          ; RPad, [ win_en <--. 1 ]
          ; LastRow, [ win_en <--. 1 ]
          ; BFirstRow, [ win_en <--. 1 ]
          ; BLPad, [ win_en <--. 1 ]
          ; BActive, [ win_en <--. 1 ]
          ; BRPad, [ win_en <--. 1 ]
          ; BLastRow, [ win_en <--. 1 ]
          ]
          ~default:[ win_en <--. 0 ]
      ; when_
          win_en.value
          [ (* end of row *)
            if_
              (col_cnt +:. (C.m - 1) >=:. C.num_cols + 1)
              [ next_col_cnt <--. 0; row_cnt <-- row_cnt.value +:. 1 ]
              [ next_col_cnt <-- col_cnt +:. C.m ]
          ]
      ; when_
          (sm.is LastRow |: sm.is BLastRow &: (col_cnt +:. (C.m - 1) >=:. C.num_cols + 1))
          [ next_col_cnt <--. 0; row_cnt <--. 0 ]
      ; sm.switch
          [ Idle, [ when_ i.start [ ready_o <--. 0; sm.set_next FirstRow ] ]
          ; ( FirstRow
            , [ when_
                  (col_cnt +:. (C.m - 1) >=:. C.num_cols + 1)
                  [ (if C.m = 1 then ready_o <--. 0 else ready_o <--. 1)
                  ; sm.set_next LPad
                  ]
              ] )
          ; ( LPad
            , if C.m = 1
              then [ ready_o <--. 1; sm.set_next Active ]
              else [ when_ i.valid [ sm.set_next Active ] ] )
          ; Active, [ when_ is_end_row [ ready_o <--. 0; sm.set_next RPad ] ]
          ; ( RPad
            , [ if_
                  is_last_row
                  [ ready_o <-- gnd; sm.set_next LastRow ]
                  [ (if C.m = 1 then ready_o <-- gnd else ready_o <-- vdd)
                  ; sm.set_next LPad
                  ]
              ] )
          ; ( LastRow
            , [ when_ (col_cnt +:. (C.m - 1) >=:. C.num_cols + 1) [ sm.set_next Wait ] ] )
          ; Wait, [ switch_inputs <--. 1; sm.set_next Check ]
          ; ( Check
            , [ if_
                  was_removed.value
                  [ sm.set_next BFirstRow ]
                  [ done_o <--. 1; sm.set_next Done ]
              ] )
          ; ( BFirstRow
            , [ when_ (col_cnt +:. (C.m - 1) >=:. C.num_cols + 1) [ sm.set_next BLPad ] ]
            )
          ; BLPad, [ sm.set_next BActive ]
          ; BActive, [ when_ is_end_row [ sm.set_next BRPad ] ]
          ; BRPad, [ if_ is_last_row [ sm.set_next BLastRow ] [ sm.set_next BLPad ] ]
          ; ( BLastRow
            , [ when_ (col_cnt +:. (C.m - 1) >=:. C.num_cols + 1) [ sm.set_next Check ] ]
            )
          ; BWait, [ sm.set_next Check ]
          ; Done, []
          ]
      ];
    { O.ready = ready_o.value; O.ddone = done_o.value; O.result = remove_cnt.value }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"day4" create
  ;;
end
