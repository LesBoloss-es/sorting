open Genlib
open BenchTable

let powersort = NearlyOptimalMergesort.PowerSort.sort

let log2_fact n =
  let rec log2_fact res = function
    | 0 -> res
    | n -> log2_fact (res +. log (float_of_int n)) (n - 1)
  in
  log2_fact 0. n /. log 2. |> ceil |> int_of_float

let bench_one ~measure ~algorithm ~inputs () =
  let inputs = List.map Array.copy inputs in
  measure (fun cmp -> List.iter (algorithm cmp) inputs)

let runtime f =
  let open Unix in
  let before = times () in
  f Int.compare;
  let after = times () in
  after.tms_utime -. before.tms_utime

let comparisons f =
  let count = ref 0 in
  let int_compare_count a b = incr count; Int.compare a b in
  f int_compare_count;
  float_of_int !count

let speedup ~from ~to_ = (from -. to_) /. from *. 100.

let bench_table ~generator ~lengths ~repeat ()
  =
  List.map
    (fun length ->
       let inputs = List.init repeat (fun _ -> generator length) in

       let t_std = lazy (bench_one ~measure:runtime ~algorithm:Array.stable_sort ~inputs ()) in
       let t_ts  = lazy (bench_one ~measure:runtime ~algorithm:powersort ~inputs ()) in
       let c_std = lazy (bench_one ~measure:comparisons ~algorithm:Array.stable_sort ~inputs ()) in
       let c_ts  = lazy (bench_one ~measure:comparisons ~algorithm:powersort ~inputs ()) in

       let repeat = float_of_int repeat in

       [ int_cell length;
         fmt_cell (fun () -> Lazy.force t_std /. repeat) ~fmt:"%.6f";
         fmt_cell (fun () -> Lazy.force t_ts  /. repeat) ~fmt:"%.6f";
         fmt_cell (fun () -> speedup ~from:(Lazy.force t_std) ~to_:(Lazy.force t_ts)) ~fmt:"%.1f%%";
         int_cell (log2_fact length) ;
         fmt_cell (fun () -> Lazy.force c_std /. repeat) ~fmt:"%.0f";
         fmt_cell (fun () -> Lazy.force c_ts  /. repeat) ~fmt:"%.0f";
         fmt_cell (fun () -> speedup ~from:(Lazy.force c_std) ~to_:(Lazy.force c_ts)) ~fmt:"%.1f%%" ])
    lengths

let headers =
  List.map string_cell [
    "length";
    "stdlib"; "powersort"; "speedup";
    "lg(n!)"; "stdlib"; "powersort"; "speedup"
  ]

let cell_styles () =
  [
    (Auto (ref 6), Right);
    (Auto (ref 8), Right);
    (Auto (ref 8), Right);
    (Auto (ref 7), Right);
    (Auto (ref 7), Right);
    (Auto (ref 7), Right);
    (Auto (ref 7), Right);
    (Auto (ref 7), Right);
  ]

let bench_table =
  let lengths = List.init 5 (fun i -> (i+ 1) * 30000) in
  let repeat = 50 in
  bench_table ~lengths ~repeat

let () = print_newline ()

let () =
  bench_table ~generator:Genarray.gen_unif ()
  |> print_table ~title:"Unif" ~headers
    ~cell_styles:(cell_styles ())

let () = print_newline ()

let () =
  bench_table ~generator:(Genarray.gen_k_runs 5) ()
  |> print_table ~title:"5-Runs" ~headers
    ~cell_styles:(cell_styles ())

let () = print_newline ()
