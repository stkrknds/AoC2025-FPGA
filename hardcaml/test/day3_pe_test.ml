open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

module TestConfig = struct
  let num_digits = 15
  let num_lines = 4
  let select_digits = 12
end

module PE = Aoc25.Day_3_pe.Make (TestConfig)
module Harness = Cyclesim_harness.Make (PE.I) (PE.O)

let ( <--. ) = Bits.( <--. )
let sssi1 = [ 9; 8; 7; 6; 5; 4; 3; 2; 1; 1; 1; 1; 1; 1; 1 ]
let sssi2 = [ 8; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 9 ]
let sssi3 = [ 2; 3; 4; 2; 3; 4; 2; 3; 4; 2; 3; 4; 2; 7; 8 ]
let sssi4 = [ 8; 1; 8; 1; 8; 1; 9; 1; 1; 1; 1; 2; 1; 1; 1 ]

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one value *)
  let feed_input n =
    inputs.data_in <--. n;
    inputs.data_in_valid := Bits.vdd;
    while not (Bits.to_bool !(outputs.ready)) do
      cycle ()
    done;
    cycle ();
    inputs.data_in_valid := Bits.gnd
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Input some data *)
  List.iter sssi1 ~f:(fun x -> feed_input x);
  List.iter sssi2 ~f:(fun x -> feed_input x);
  List.iter sssi3 ~f:(fun x -> feed_input x);
  List.iter sssi4 ~f:(fun x -> feed_input x);
  while not (Bits.to_bool !(outputs.done_o)) do
    cycle ()
  done;
  printf "Result: %d\n" (Bits.to_int_trunc !(outputs.acc))
;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)

(* let waves_config =  *)
(*  Waves_config.to_directory ""  *)
(*  |> Waves_config.as_wavefile_format ~format:Vcd *)

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Harness.run_advanced ~create:PE.hierarchical simple_testbench;
  [%expect {| Result: 3121910778619 |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  (* For simple tests, we can print the waveforms directly in an expect-test (and use the
     command [dune promote] to update it after the tests run). This is useful for quickly
     visualizing or documenting a simple circuit, but limits the amount of data that can
     be shown. *)
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "pe*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:PE.hierarchical
    ~trace:`Everything
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:92
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    simple_testbench;
  [%expect
    {|
    Result: 3121910778619
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │                            ││────────────────────────────────────────────────────────────│
    │pe$acc                      ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │pe$acc_1                    ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │pe$count                    ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────┬───────────┬───────────┬───────────┬───────│
    │pe$digit_cnt                ││ 0              │1          │2          │3          │4      │
    │                            ││────────────────┴───────────┴───────────┴───────────┴───────│
    │pe$done_o                   ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │pe$i$acc_in                 ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │pe$i$clear                  ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │pe$i$clock                  ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │                            ││────────────┬───┬───────────┬───────────┬───────────┬───────│
    │pe$i$data_in                ││ 0          │9  │8          │7          │6          │5      │
    │                            ││────────────┴───┴───────────┴───────────┴───────────┴───────│
    │pe$i$data_in_valid          ││            ┌───────────────────────────────────────────────│
    │                            ││────────────┘                                               │
    │pe$i$do_acc                 ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │pe$i$push_acc               ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │pe$i$start                  ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │pe$is_stack_empty           ││            ┌───────┐                                       │
    │                            ││────────────┘       └───────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │pe$lacc_cnt                 ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │pe$line_cnt                 ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │pe$o$acc                    ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │pe$o$done_o                 ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │pe$o$ready                  ││            ┌───┐       ┌───┐       ┌───┐       ┌───┐       │
    │                            ││────────────┘   └───────┘   └───────┘   └───────┘   └───────│
    │                            ││────────────────┬───────────┬───────────┬───────────┬───────│
    │pe$rdata                    ││ 0              │9          │8          │7          │6      │
    │                            ││────────────────┴───────────┴───────────┴───────────┴───────│
    │pe$ready                    ││            ┌───┐       ┌───┐       ┌───┐       ┌───┐       │
    │                            ││────────────┘   └───────┘   └───────┘   └───────┘   └───────│
    │                            ││────────────┬───────────────────────────────────────────────│
    │pe$remove_num               ││ 0          │3                                              │
    │                            ││────────────┴───────────────────────────────────────────────│
    │                            ││────────────────────┬───────────┬───────────┬───────────┬───│
    │pe$stack_idx                ││ 0                  │1          │2          │3          │4  │
    │                            ││────────────────────┴───────────┴───────────┴───────────┴───│
    │pe$start_acc                ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │pe$zero_acc                 ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}]
;;
