let filepath = "/home/stratos/projects/aoc2025/fpga/hardcaml/input4.txt"

(* Set these based on the input file *)
(* I tested with my 140x140 input *)
(* if it gets too big, the addresses' width would have to be changed *)
module TestConfig = struct
  let num_rows = 140
  let num_cols = 140
  let m = 2
end

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Day4 = Aoc25.Day4.Make (TestConfig)
module Harness = Cyclesim_harness.Make (Day4.I) (Day4.O)

let ( <--. ) = Bits.( <--. )

let test_grid =
  [ [ 0; 0; 1; 1; 0; 1; 1; 1; 1; 0 ]
  ; [ 1; 1; 1; 0; 1; 0; 1; 0; 1; 1 ]
  ; [ 1; 1; 1; 1; 1; 0; 1; 0; 1; 1 ]
  ; [ 1; 0; 1; 1; 1; 1; 0; 0; 1; 0 ]
  ; [ 1; 1; 0; 1; 1; 1; 1; 0; 1; 1 ]
  ; [ 0; 1; 1; 1; 1; 1; 1; 1; 0; 1 ]
  ; [ 0; 1; 0; 1; 0; 1; 0; 1; 1; 1 ]
  ; [ 1; 0; 1; 1; 1; 0; 1; 1; 1; 1 ]
  ; [ 0; 1; 1; 1; 1; 1; 1; 1; 1; 0 ]
  ; [ 1; 0; 1; 0; 1; 1; 1; 0; 1; 0 ]
  ]
;;

(* AI GENERATED FUNCTION *)
let read_matrix_file filepath =
  In_channel.with_file filepath ~f:(fun ic ->
    In_channel.input_lines ic
    |> List.filter ~f:(fun line -> not (String.is_empty line))
    |> List.map ~f:(fun line ->
      line
      |> String.to_list
      |> List.filter_map ~f:(function
        | '@' -> Some 1
        | '.' -> Some 0
        | _ -> None (* Automatically ignores newlines/spaces *))))
;;

let simple_testbench (sim : Harness.Sim.t) =
  printf "DAY 4 PART 2 TEST \n";
  let input_grid = read_matrix_file filepath in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let cycles = ref 0 in
  (* Define a helper to use instead of Cyclesim.cycle *)
  let tick () =
    Cyclesim.cycle sim;
    Int.incr cycles
  in
  (* Helper function for inputting one value *)
  let feed_input n =
    inputs.data_in := n;
    inputs.valid := Bits.vdd;
    (* Add debug print here to see if it's set *)
    while not (Bits.to_bool !(outputs.ready)) do
      tick ()
    done;
    tick ();
    inputs.valid := Bits.gnd
  in
  (* AI GENERATED FUNCTION*)
  let feed_grid grid ~chunk_size =
    let pairs =
      List.concat_map grid ~f:(fun row ->
        (* 1. Split the row into sub-lists of size m *)
        let chunks = List.chunks_of row ~length:chunk_size in
        (* 2. Convert each chunk (e.g., [0; 1]) into a string (e.g., "01") *)
        List.map chunks ~f:(fun chunk -> List.map chunk ~f:Int.to_string |> String.concat))
    in
    List.iteri pairs ~f:(fun _ chunk -> feed_input (Hardcaml.Bits.of_string chunk))
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  tick ();
  inputs.clear := Bits.gnd;
  tick ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  tick ();
  inputs.start := Bits.gnd;
  (* Input some data *)
  feed_grid input_grid ~chunk_size:TestConfig.m;
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ();
  while not (Bits.to_bool !(outputs.ddone)) do
    tick ()
  done;
  printf "HARDWARE RESULT %d \n" (Bits.to_int_trunc !(outputs.result));
  printf "Simulation completed in %d cycles.\n%!" !cycles
;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)

(*let waves_config =
  Waves_config.to_directory ""
  |> Waves_config.as_wavefile_format ~format:Vcd
;;*)

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

let%expect_test "Simple test, day4" =
  Printf.printf "1. Starting feed_array...%!";
  Harness.run_advanced ~create:Day4.hierarchical simple_testbench;
  [%expect
    {|
    1. Starting feed_array...DAY 4 PART 2 TEST
    HARDWARE RESULT 9173
    Simulation completed in 927638 cycles.
    |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  (* For simple tests, we can print the waveforms directly in an expect-test (and use the
     command [dune promote] to update it after the tests run). This is useful for quickly
     visualizing or documenting a simple circuit, but limits the amount of data that can
     be shown. *)
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day4*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Day4.hierarchical
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
    DAY 4 PART 2 TEST
    HARDWARE RESULT 9173
    Simulation completed in 927638 cycles.
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │day4$i$clear                ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │day4$i$clock                ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │                            ││────────────┬───────────────────────────────────────────────│
    │day4$i$data_in              ││ 0          │2                                              │
    │                            ││────────────┴───────────────────────────────────────────────│
    │day4$i$start                ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │day4$i$valid                ││            ┌───────────────────────────────────────────────│
    │                            ││────────────┘                                               │
    │                            ││────────────────────────────────────────────────────────────│
    │day4$next_br_addr           ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───│
    │day4$next_col_cnt           ││ 0          │2  │4  │6  │8  │10 │12 │14 │16 │18 │20 │22 │24 │
    │                            ││────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───│
    │day4$o$ddone                ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │day4$o$ready                ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │day4$o$result               ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │day4$remove_cnt             ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────│
    │day4$row_cnt                ││ 0                                                          │
    │                            ││────────────────────────────────────────────────────────────│
    │day4$switch_inputs          ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │day4$was_removed            ││                                                            │
    │                            ││────────────────────────────────────────────────────────────│
    │day4$win_en                 ││            ┌───────────────────────────────────────────────│
    │                            ││────────────┘                                               │
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}]
;;
