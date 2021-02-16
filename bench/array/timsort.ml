open Sorting_array
open Genlib

let pf = Format.printf
let fpf = Format.fprintf
let spf = Format.sprintf

let pp_int = Format.pp_print_int
let pp_string = Format.pp_print_string

type cell = Cell : (unit -> 'a) * ('a -> string) -> cell
type row = cell list
type table = row list

let cell ~to_string f = Cell (f, to_string)

let fmt_cell ~fmt f = cell ~to_string:(spf fmt) f

let int_cell i = cell ~to_string:string_of_int (fun () -> i)
let string_cell s = cell ~to_string:Fun.id (fun () -> s)

type cell_size = None | Given of int | Auto of int ref
type align = Left | Center | Right

let fill_string ?(fill=' ') size = String.make size fill

let align_string ?fill align str size =
  let len = String.length str in
  match align with
  | Left -> str ^ (fill_string ?fill (size - len))
  | Right -> (fill_string ?fill (size - len)) ^ str
  | Center -> (fill_string ?fill ((size - len) / 2)) ^ str ^ (fill_string ?fill (size - len - (size - len) / 2))

let pp_cell ~size ~align fmt = function
  | Cell (f, to_string) ->
    let str = to_string (f ()) in
    let len = String.length str in
    let size =
      match size with
      | None -> len
      | Auto size -> size := max !size len; !size
      | Given size -> size
    in
    let str = align_string align str size in
    pp_string fmt str

let pp_row ~sep_len ?cell_styles fmt cells =
  match cells with
  | [] -> ()
  | cell :: cells ->
    let ((size, align), styles) =
      match cell_styles with
      | None -> ((None, Left), List.map (fun _ -> (None, Left)) cells)
      | Some ((size, align) :: styles) -> ((size, align), styles)
      | _ -> invalid_arg "pp_row"
    in
    pp_cell ~size ~align fmt cell;

    List.iter2
      (fun cell (size, align) ->
         fpf fmt "%s@?" (String.make sep_len ' ');
         pp_cell ~size ~align fmt cell)
      cells styles;
    fpf fmt "@."

let pp_table ?title ?headers ?(sep_len=3) ?cell_styles fmt t =
  let len () =
    match cell_styles with
    | None -> 0
    | Some cell_styles ->
      List.fold_left
        (fun len (size, _align) ->
           len + sep_len
           + match size with
             | None -> 0
             | Auto size -> !size
             | Given size -> size)
        (-sep_len) cell_styles
  in
  (match title with
   | None -> ()
   | Some title -> fpf fmt "%s@." (align_string Center (spf " [ %s ] " title) (len ()) ~fill:'='));
  (match headers with
   | None -> ()
   | Some headers -> pp_row ~sep_len ?cell_styles fmt headers);
  fpf fmt "%s@." (String.make (len ()) '-');
  List.iter (pp_row ~sep_len ?cell_styles fmt) t;
  fpf fmt "%s@." (String.make (len ()) '-')

let print_table ?title ?headers ?cell_styles t =
  pp_table ?title ?headers ?cell_styles Format.std_formatter t

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
       let t_ts  = lazy (bench_one ~measure:runtime ~algorithm:Timsort.timsort ~inputs ()) in
       let c_std = lazy (bench_one ~measure:comparisons ~algorithm:Array.stable_sort ~inputs ()) in
       let c_ts  = lazy (bench_one ~measure:comparisons ~algorithm:Timsort.timsort ~inputs ()) in

       [ int_cell length;
         fmt_cell (fun () -> Lazy.force t_std) ~fmt:"%.6f";
         fmt_cell (fun () -> Lazy.force t_ts)  ~fmt:"%.6f";
         fmt_cell (fun () -> speedup ~from:(Lazy.force t_std) ~to_:(Lazy.force t_ts)) ~fmt:"%.1f%%";
         int_cell (repeat * log2_fact length) ;
         fmt_cell (fun () -> Lazy.force c_std) ~fmt:"%.0f";
         fmt_cell (fun () -> Lazy.force c_ts)  ~fmt:"%.0f";
         fmt_cell (fun () -> speedup ~from:(Lazy.force c_std) ~to_:(Lazy.force c_ts)) ~fmt:"%.1f%%" ])

    lengths

let headers =
  List.map string_cell [
    "length";
    "stdlib"; "timsort"; "speedup";
    "lg(n!)"; "stdlib"; "timsort"; "speedup"
  ]

let cell_styles () =
  [
    (Auto (ref 6), Right);
    (Auto (ref 8), Right);
    (Auto (ref 8), Right);
    (Auto (ref 7), Right);
    (Auto (ref 8), Right);
    (Auto (ref 8), Right);
    (Auto (ref 8), Right);
    (Auto (ref 7), Right);
  ]

let bench_table =
  let lengths = List.init 15 (fun i -> (i+ 1) * 10000) in
  let repeat = 20 in
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
