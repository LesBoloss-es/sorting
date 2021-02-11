open Sorting_array
open Genlib

let time f =
  let open Unix in
  let before = times () in
  let x = f () in
  let after = times () in
  after.tms_utime -. before.tms_utime, x

let bench gen sort ~repeat ~len =
  let ts = List.init repeat (fun _ -> gen len) in
  let runtime, () = time (fun () -> List.iter sort ts) in
  runtime, ts

let runtime gen =
  let repeat = 20 in
  Format.printf "len\tstdlib\ttimsort\tspeedup@.";
  for i = 1 to 15 do
    let len = i * 10000 in
    let state = Random.get_state () in
    let stdlib, _ = bench gen (Array.stable_sort Int.compare) ~repeat ~len in
    Random.set_state state;
    let us, _ = bench gen (Timsort.timsort Int.compare) ~repeat ~len in
    let speedup = (stdlib -. us) /. stdlib *. 100. in
    Format.printf "%d\t%.6f\t%.6f\t%.1f@." len stdlib us speedup
  done

let comparisons gen =
  let repeat = 20 in
  let counter = ref 0 in
  let compare_count i j =
    incr counter;
    Int.compare i j
  in
  Format.printf "len\tstdlib\ttimsort\tspeedup@.";
  for i = 1 to 15 do
    let len = i * 10000 in
    let state = Random.get_state () in
    counter := 0;
    let _ = bench gen (Array.stable_sort compare_count) ~repeat ~len in
    let stdlib = !counter in
    Random.set_state state;
    counter := 0;
    let _ = bench gen (Timsort.timsort compare_count) ~repeat ~len in
    let us = !counter in
    let foi = float_of_int in
    let speedup = (foi stdlib -. foi us) /. (foi stdlib) *. 100. in
    Format.printf "%d\t%d\t%d\t%.1f@." len stdlib us speedup
  done


let () =
  Format.printf "--- Unif ---@.";
  runtime Genarray.gen_unif;
  Format.printf "-----------@.";
  comparisons Genarray.gen_unif;
  Format.printf "--- 5-runs ---@.";
  runtime (Genarray.gen_k_runs 5);
  Format.printf "-----------@.";
  comparisons (Genarray.gen_k_runs 5);
