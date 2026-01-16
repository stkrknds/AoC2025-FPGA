module type Config = sig
  val num_digits : int
  val num_lines : int
  val select_digits : int
end

module Make (C : Config) = struct
  open! Core
  open! Hardcaml
  open! Signal

  let num_bits = 4
  let avail_removes = C.num_digits - C.select_digits

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; acc_in : 'a [@bits 64]
      ; push_acc : 'a
      ; do_acc : 'a
      ; data_in : 'a [@bits num_bits]
      ; data_in_valid : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ready : 'a
      ; done_o : 'a
      ; acc : 'a [@bits 64]
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | Check
      | Check1
      | Ready
      | Pop
      | Push
      | Acc
      | Wait
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create
    scope
    ({ clock; clear; start; data_in; data_in_valid; push_acc; do_acc; acc_in } : _ I.t)
    : _ O.t
    =
    (* bcd to bin taken from https://www.janestreet.com/web-app/hardcaml-docs/more-on-circuit-design/binary_coded_decimal/#converting-to-binary *)
    (* Find the number of digits to convert and create a counter. *)
    let bcd2bin spec bcd start_acc =
      let digits = split_msb ~part_width:4 bcd in
      let num_digits = List.length digits in
      (* We only support multiple digits - for a single digit the output equals the input
        anyway. *)
      assert (num_digits > 1);
      let%hw_var count = Always.Variable.reg spec ~width:(Int.ceil_log2 num_digits) in
      let valid = Always.Variable.reg spec ~width:1 in
      (* Create an accumulator large enough to hold the final result. *)
      let max_result = Int.pow 10 num_digits - 1 in
      let%hw_var acc =
        Always.Variable.reg spec ~width:(num_bits_to_represent max_result)
      in
      (* Select the current digit being processed. Note that when we split [bcd] into [digits]
        we did so from the top most digit. *)
      let digit = mux count.value digits |> uresize ~width:(width acc.value) in
      Always.(
        compile
          [ valid <--. 0
          ; if_
              (count.value ==:. 0)
              [ (* Wait for start. When applied set the accumulator with the top most digit.
                *)
                when_ start_acc [ count <-- count.value +:. 1; acc <-- digit ]
              ]
              [ (* Add each succesive digit to the accumulator times 10. *)
                count <-- count.value +:. 1
              ; acc
                <-- drop_top (acc.value *: of_unsigned_int ~width:4 10) ~width:4 +: digit
              ; (* Finished processing digits. *)
                when_ (count.value ==:. num_digits - 1) [ count <--. 0; valid <--. 1 ]
              ]
          ]);
      acc.value, valid.value
    in
    let open Always in
    let spec = Reg_spec.create ~clock ~clear () in
    let sm = State_machine.create (module States) spec in
    (* the number of digits we can remove*)
    let%hw_var remove_num = Variable.reg spec ~width:16 in
    let%hw_var ready = Variable.reg spec ~width:1 in
    let%hw_var done_o = Variable.reg spec ~width:1 in
    let%hw_var digit_cnt = Variable.reg spec ~width:16 in
    let%hw_var line_cnt = Variable.reg spec ~width:16 in
    let%hw_var rdata = Variable.reg spec ~width:4 in
    (* starts the bcd conversion *)
    let%hw_var start_acc = Variable.reg spec ~width:1 in
    let%hw_var acc = Variable.reg spec ~width:64 in
    let%hw_var lacc_cnt = Variable.reg spec ~width:8 in
    let stack = Array.init C.select_digits ~f:(fun _ -> Variable.reg spec ~width:4) in
    let%hw_var is_stack_empty = Variable.reg spec ~width:1 in
    let%hw_var stack_idx = Variable.reg spec ~width:9 in
    let stack_signals = Array.map stack ~f:(fun v -> v.value) in
    let combined = concat_msb (Array.to_list stack_signals) in
    let b2b_num, b2b_valid = bcd2bin spec combined start_acc.value in
    (* acc signals *)
    (* zero_acc: resets the acc *)
    (* push_acc: passes the acc in the input directly to the output *)
    (* do_acc: we accumulate the acc with the one in the input *)
    let%hw_var zero_acc = Variable.reg spec ~width:1 in
    (* Points at the top of the stack *)
    let top_stack =
      mux (stack_idx.value -:. 1) (Array.to_list (Array.map stack ~f:(fun r -> r.value)))
    in
    compile
      [ if_
          zero_acc.value
          [ acc <--. 0; lacc_cnt <--. 0 ]
          [ when_
              b2b_valid
              [ acc <-- acc.value +: uresize b2b_num ~width:(width acc.value)
              ; lacc_cnt <-- lacc_cnt.value +:. 1
              ]
          ; when_ push_acc [ acc <-- acc_in ]
          ; when_ do_acc [ acc <-- acc.value +: acc_in ]
          ]
      ];
    compile
      [ sm.switch
          [ ( Idle
            , [ zero_acc <--. 1
              ; when_
                  start
                  [ start_acc <--. 0
                  ; done_o <--. 0
                  ; line_cnt <--. 0
                  ; zero_acc <--. 0
                  ; digit_cnt <--. 0
                  ; stack_idx <--. 0
                  ; remove_num <--. avail_removes
                  ; ready <--. 1
                  ; is_stack_empty <--. 1
                  ; sm.set_next Ready
                  ]
              ] )
          ; ( Ready
            , [ when_
                  data_in_valid
                  [ rdata <-- data_in
                  ; ready <--. 0
                  ; digit_cnt <-- digit_cnt.value +:. 1
                  ; if_
                      is_stack_empty.value
                      [ sm.set_next Push ]
                      [ if_
                          (data_in >: top_stack &: (remove_num.value <>:. 0))
                          [ sm.set_next Pop ]
                          [ (* If stack isn't full we push *)
                            if_
                              (stack_idx.value <:. C.select_digits)
                              [ sm.set_next Push ]
                              [ when_
                                  (remove_num.value >:. 0)
                                  [ remove_num <-- remove_num.value -:. 1 ]
                              ; sm.set_next Check
                              ]
                          ]
                      ]
                  ]
              ] )
          ; ( Check
            , [ if_
                  (digit_cnt.value ==:. C.num_digits)
                  [ start_acc <--. 1; sm.set_next Acc ]
                  [ ready <--. 1; sm.set_next Ready ]
              ] )
          ; ( Pop
            , [ if_
                  is_stack_empty.value
                  [ sm.set_next Push ]
                  [ if_
                      (rdata.value >: top_stack &: (remove_num.value <>:. 0))
                      [ stack_idx <-- stack_idx.value -:. 1
                      ; when_ (stack_idx.value ==:. 1) [ is_stack_empty <--. 1 ]
                      ; remove_num <-- remove_num.value -:. 1
                      ]
                      [ sm.set_next Push ]
                  ]
              ] )
          ; ( Push
            , [ (* push the digit in the stack and increment the index *)
                proc
                  (List.init C.select_digits ~f:(fun idx ->
                     when_ (stack_idx.value ==:. idx) [ stack.(idx) <-- rdata.value ]))
              ; stack_idx <-- stack_idx.value +:. 1
              ; is_stack_empty <--. 0
              ; sm.set_next Check
              ] )
          ; Acc, [ start_acc <--. 0; sm.set_next Check1 ]
          ; ( Check1
            , [ if_
                  (line_cnt.value ==:. C.num_lines - 1)
                  [ sm.set_next Wait ]
                  [ ready <--. 1
                  ; digit_cnt <--. 0
                  ; stack_idx <--. 0
                  ; remove_num <--. avail_removes
                  ; is_stack_empty <--. 1
                  ; line_cnt <-- line_cnt.value +:. 1
                  ; sm.set_next Ready
                  ]
              ] )
          ; ( Wait
            , [ when_
                  (lacc_cnt.value ==:. C.num_lines)
                  [ done_o <--. 1; sm.set_next Done ]
              ] )
          ; Done, []
          ]
      ];
    { O.ready = ready.value; O.done_o = done_o.value; O.acc = acc.value }
  ;;

  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"pe" create
  ;;
end
