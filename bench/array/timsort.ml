open Sorting_array
open Genlib

let pf = Format.printf
let fpf = Format.fprintf
let spf = Format.sprintf

let pp_int = Format.pp_print_int
let pp_string = Format.pp_print_string

type cell = Cell : (unit -> 'a) * (Format.formatter -> 'a -> unit) -> cell
type row = cell list
type table = row list

let cell ~pp f = Cell (f, pp)

let fmt_cell ~fmt f = cell ~pp:(fun ppf x -> fpf ppf fmt x) f

let int_cell i = cell ~pp:pp_int (fun () -> i)
let string_cell s = cell ~pp:pp_string (fun () -> s)

let pp_cell fmt = function
  | Cell (f, pp) -> pp fmt (f ())

let pp_row fmt = function
  | [] -> ()
  | cell :: cells ->
    pp_cell fmt cell;
    List.iter (fun cell -> fpf fmt " @?"; pp_cell fmt cell) cells;
    fpf fmt "@."

let pp_table fmt = List.iter (pp_row fmt)

let print_table t = pp_table Format.std_formatter t

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
  let headers =
    List.map string_cell [
      "length";
      "stdlib"; "timsort"; "speedup";
      "lg(n!)"; "stdlib"; "timsort"; "speedup"
    ]
  in
  List.map
    (fun length ->
       let inputs = List.init repeat (fun _ -> generator length) in

       let t_std = lazy (bench_one ~measure:runtime ~algorithm:Array.stable_sort ~inputs ()) in
       let t_ts  = lazy (bench_one ~measure:runtime ~algorithm:Timsort.timsort ~inputs ()) in
       let c_std = lazy (bench_one ~measure:comparisons ~algorithm:Array.stable_sort ~inputs ()) in
       let c_ts  = lazy (bench_one ~measure:comparisons ~algorithm:Timsort.timsort ~inputs ()) in

       [ int_cell length;
         fmt_cell (fun () -> Lazy.force t_std) ~fmt:"%.6f";
         fmt_cell (fun () -> Lazy.force t_ts)  ~fmt:"%.6f";
         fmt_cell (fun () -> speedup ~from:(Lazy.force t_std) ~to_:(Lazy.force t_ts)) ~fmt:"%.1f";
         int_cell (repeat * log2_fact length) ;
         fmt_cell (fun () -> Lazy.force c_std) ~fmt:"%.0f";
         fmt_cell (fun () -> Lazy.force c_ts)  ~fmt:"%.0f";
         fmt_cell (fun () -> speedup ~from:(Lazy.force c_std) ~to_:(Lazy.force c_ts)) ~fmt:"%.1f" ])

    lengths
  |> List.cons headers

let bench_table =
  let lengths = List.init 15 (fun i -> (i+ 1) * 10000) in
  let repeat = 20 in
  bench_table ~lengths ~repeat

let () =
  pf "============== [ Unif ] ==============@.";
  bench_table ~generator:Genarray.gen_unif () |> print_table

let () =
  pf "============= [ 5-Runs ] =============@.";
  bench_table ~generator:(Genarray.gen_k_runs 5) () |> print_table
