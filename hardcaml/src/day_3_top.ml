module type Config = sig
  val num_digits : int
  val num_lines : int
  val select_digits : int
  val m : int
end

module Make (C : Config) = struct
  open! Core
  open! Hardcaml
  open! Signal

  let num_digits = 15
  let bus_width = 4 * C.m

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; valid : 'a
      ; data_in : 'a [@bits bus_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ready : 'a
      ; ddone : 'a
      ; result : 'a [@bits 64]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | Start
      | Wait
      | Acc
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module PeConfig = struct
    let num_digits = C.num_digits
    let num_lines = C.num_lines / C.m
    let select_digits = C.select_digits
  end

  module PE = Day_3_pe.Make (PeConfig)

  let create scope (i : Signal.t I.t) : Signal.t O.t =
    let open Always in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = State_machine.create (module States) spec in
    (* PE signals *)
    let pe_start = Always.Variable.reg spec ~width:1 in
    let pe_push_acc = Array.init C.m ~f:(fun _ -> Variable.wire ~default:(zero 1) ()) in
    let pe_ready = Array.init C.m ~f:(fun _ -> Variable.wire ~default:(zero 1) ()) in
    let pe_data = Array.init C.m ~f:(fun _ -> Variable.wire ~default:(zero 4) ()) in
    let pe_done = Array.init C.m ~f:(fun _ -> Variable.wire ~default:(zero 1) ()) in
    let pe_valid = Variable.wire ~default:(zero 1) () in
    let pe_do_acc = Array.init C.m ~f:(fun _ -> Variable.wire ~default:(zero 1) ()) in
    (* The result of the rightmost pe *)
    let result = Variable.wire ~default:(zero 64) () in
    let digit_cnt = Always.Variable.reg spec ~width:16 in
    let done_o = Always.Variable.reg spec ~width:1 in
    (* After all input digits are processed, the partial results *)
    (* must propagate through the PE chain. An additional m - 1 *)
    (* cycles are required for the final result to reach the output. *)
    let wait_cnt = Always.Variable.reg spec ~width:8 in
    (* we only received data when all the PEs are ready *)
    (* this of course isn't the best but it's simple *)
    let ready_o =
      pe_ready
      |> Array.to_list
      |> List.map ~f:(fun v -> v.value)
      |> Signal.reduce ~f:Signal.( &: )
    in
    (* We will wait until every PE is done processing digits *)
    let pe_all_done =
      pe_done
      |> Array.to_list
      |> List.map ~f:(fun v -> v.value)
      |> Signal.reduce ~f:Signal.( &: )
    in
    (* Chain instances using fold *)
    let final_output, final_acc =
      List.fold
        (List.init C.m ~f:Fn.id)
        ~init:([], zero 64)
        ~f:(fun (outputs_acc, data_in) idx ->
          let stage_scope = Scope.sub_scope scope (sprintf "pe_%d" idx) in
          let stage_inputs =
            { PE.I.clock = i.clock
            ; clear = i.clear
            ; data_in = pe_data.(idx).value
            ; data_in_valid = pe_valid.value
            ; acc_in = data_in
            ; start = pe_start.value
            ; push_acc = pe_push_acc.(idx).value
            ; do_acc = pe_do_acc.(idx).value
            }
          in
          let stage_outputs = PE.hierarchical stage_scope stage_inputs in
          stage_outputs :: outputs_acc, stage_outputs.acc
          (* Collect outputs *))
    in
    let stage_outputs = Array.of_list (List.rev final_output) in
    (* map the input to the PEs *)
    (* 4-bit data slice per PE *)
    let slice_assignments =
      Array.to_list
        (Array.mapi pe_data ~f:(fun idx wire ->
           let low = idx * 4 in
           let high = ((idx + 1) * 4) - 1 in
           (* Slice the input and assign to the wire at index i *)
           wire <-- select i.data_in ~high ~low))
    in
    compile
      [ (* TODO fix later *)
        (* this is a hack: data are only valid(received) when every PE is ready *)
        pe_valid <-- (ready_o &: i.valid) (* Connect signals *)
      ; proc (List.init C.m ~f:(fun idx -> pe_push_acc.(idx) <--. 0))
      ; proc (List.init C.m ~f:(fun idx -> pe_do_acc.(idx) <--. 0))
      ; proc slice_assignments
      ; proc (List.init C.m ~f:(fun idx -> pe_ready.(idx) <-- stage_outputs.(idx).ready))
      ; result <-- final_acc
      ; proc (List.init C.m ~f:(fun idx -> pe_done.(idx) <-- stage_outputs.(idx).done_o))
        (* FSM *)
        (* We mostly wait for the PEs to do their job *)
        (* Once they are done we push the results across the row *)
        (* We wait for m - 1 cycles until the result reaches the end of the row *)
      ; sm.switch
          [ ( Idle
            , [ digit_cnt <--. C.num_digits
              ; done_o <--. 0
              ; when_ i.start [ pe_start <--. 1; sm.set_next Start ]
              ] )
          ; Start, [ pe_start <--. 0; sm.set_next Wait ]
          ; Wait, [ when_ pe_all_done [ sm.set_next Acc ] ]
          ; ( Acc
            , [ proc
                  (List.init C.m ~f:(fun idx ->
                     if idx < C.m - 1
                     then pe_push_acc.(idx) <--. 1
                     else pe_push_acc.(idx) <--. 0))
              ; (if C.m > 1
                 then pe_do_acc.(C.m - 1) <--. 1
                 else pe_do_acc.(C.m - 1) <--. 0)
              ; if_
                  (wait_cnt.value +:. 1 ==:. C.m - 1)
                  [ done_o <--. 1; sm.set_next Done ]
                  [ wait_cnt <-- wait_cnt.value +:. 1 ]
              ] )
          ; Done, []
          ]
      ];
    { O.ready = ready_o; O.ddone = done_o.value; O.result = result.value }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"top" create
  ;;
end
