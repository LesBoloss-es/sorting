open Sorting_array
open Genlib

let pf = Format.printf
let fpf = Format.fprintf

let bench
    ~generator
    ~lengths ~repeat
    ~measure ~pp_measure
    ?(check_outputs=false)
    ()
  =
  pf "length\tstdlib\ttimsort\tspeedup@.";
  lengths |> List.iter @@ fun len ->
  let std_in = List.init repeat (fun _ -> generator len) in
  let ts_in  = List.map Array.copy std_in in

  let std, std_out = measure (fun cmp -> List.iter (Array.stable_sort cmp) std_in) in
  let ts,  ts_out  = measure (fun cmp -> List.iter (Timsort.timsort   cmp) ts_in)  in
  assert (not check_outputs || std_out = ts_out);
  let speedup = (std -. ts) /. std *. 100. in
  pf "%d\t%a\t%a\t%.1f%%@." len pp_measure std pp_measure ts speedup

let runtime f =
  let open Unix in
  let before = times () in
  let x = f Int.compare in
  let after = times () in
  (after.tms_utime -. before.tms_utime, x)

let comparisons f =
  let count = ref 0 in
  let int_compare_count a b = incr count; Int.compare a b in
  let x = f int_compare_count in
  (float_of_int !count, x)

let bench =
  let lengths = List.init 15 (fun i -> (i+ 1) * 10000) in
  let repeat = 20 in
  bench ~lengths ~repeat

let () =
  pf "============== [ Unif ] ==============@.";
  let bench = bench ~generator:Genarray.gen_unif in
  pf "------------- [ Runtime ] ------------@.";
  bench
    ~measure:runtime
    ~pp_measure:(fun fmt -> fpf fmt "%.6f")
    ();
  pf "----------- [ Comparisons ] ----------@.";
  bench
    ~measure:comparisons
    ~pp_measure:(fun fmt -> fpf fmt "%8.0f")
    ()

let () =
  pf "============= [ 5-Runs ] =============@.";
  let bench = bench ~generator:(Genarray.gen_k_runs 5) in
  pf "------------- [ Runtime ] ------------@.";
  bench
    ~measure:runtime
    ~pp_measure:(fun fmt -> fpf fmt "%.6f")
    ();
  pf "----------- [ Comparisons ] ----------@.";
  bench
    ~measure:comparisons
    ~pp_measure:(fun fmt -> fpf fmt "%8.0f")
    ()
