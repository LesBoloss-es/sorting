open Genlib.Genarray
open Table
open Sorting_array

let spf = Format.sprintf

let usage_msg = spf "%s [OPTION ...]\n\nOPTION can be:" Sys.argv.(0)

let default_lengths = List.init 5 (fun i -> (i+ 1) * 30000)
let lengths = ref default_lengths

let set_lengths s =
  s
  |> String.split_on_char ','
  |> List.map int_of_string
  |> (:=) lengths

let default_sorters = all_sorters
let sorters = ref default_sorters

let set_sorters s =
  s
  |> String.split_on_char ','
  |> List.map (fun sorter_name ->
      List.find (fun sorter -> sorter.name = sorter_name) all_sorters)
  |> (:=) sorters

let default_repeat = 50
let repeat = ref 50

let speclist = [
    "--lengths", Arg.String set_lengths, "INTS Lengths to use (default: ...)";
    "--sorters", Arg.String set_sorters, "STRS Sorters to use (default: ...)";
    "--repeat", Arg.Set_int repeat, spf "INT Repetitions (default: %d)" default_repeat;
  ] |> Arg.align

let anon_fun _ = assert false

let () = Arg.parse speclist anon_fun usage_msg

let log2_fact n =
  let rec log2_fact res = function
    | 0 -> res
    | n -> log2_fact (res +. log (float_of_int n)) (n - 1)
  in
  log2_fact 0. n /. log 2. |> ceil |> int_of_float

let list_concat_map f l =
  l |> List.map f |> List.flatten

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

let () =
  Format.printf "lengths:@.";
  List.iter (Format.printf "  %d@.") !lengths

let () =
  Format.printf "sorters:@.";
  List.iteri (fun i sorter -> Format.printf "  [%d] %s@." (i+1) sorter.name) !sorters

let () = Format.printf "repeat: %d@." !repeat
let () = Format.printf "@."

type result =
  { runtime : float ;
    comparisons : float }

type bench =
  { by_sorter : (string, (int, result) Hashtbl.t) Hashtbl.t ;
    by_length : (int, (string, result) Hashtbl.t) Hashtbl.t }

let new_bench () =
  { by_sorter = Hashtbl.create 8 ;
    by_length = Hashtbl.create 8 }

let add_bench ~sorter ~length ~runtime ~comparisons bench =
  let by_sorter =
    match Hashtbl.find_opt bench.by_sorter sorter.name with
    | None ->
      let tbl = Hashtbl.create 8 in
      Hashtbl.add bench.by_sorter sorter.name tbl;
      tbl
    | Some tbl -> tbl
  in
  let by_length =
    match Hashtbl.find_opt bench.by_length length with
    | None ->
      let tbl = Hashtbl.create 8 in
      Hashtbl.add bench.by_length length tbl;
      tbl
    | Some tbl -> tbl
  in
  if Hashtbl.mem by_sorter length || Hashtbl.mem by_length sorter.name then
    failwith "add_bench: this sorter and length pair is already known";
  Hashtbl.add by_sorter length { runtime ; comparisons };
  Hashtbl.add by_length sorter.name { runtime ; comparisons }

let get_bench_result ~sorter ~length bench =
  Hashtbl.find (Hashtbl.find bench.by_sorter sorter.name) length

let compute_benchs ~generator =
  let results = new_bench () in
  (
    !lengths |> List.iter @@ fun length ->
    Format.printf "[%d]@." length;
    let inputs = List.init !repeat (fun _ -> generator length) in
    (
      let nb_sorters = List.length !sorters in
      !sorters |> List.iteri @@ fun i sorter ->
      Format.printf "  [%d/%d] %s@." (i+1) nb_sorters sorter.name;
      let runtime = bench_one ~measure:runtime ~algorithm:sorter.sorter ~inputs () in
      let comparisons = bench_one ~measure:comparisons ~algorithm:sorter.sorter ~inputs () in
      let repeat = float_of_int !repeat in
      add_bench ~sorter ~length ~runtime:(runtime /. repeat *. 1000.) ~comparisons:(comparisons /. repeat) results
    )
  );
  results

let slowdown ~from ~to_ = (to_ -. from) /. from *. 100.

let reference benchs getter length =
  let by_length = Hashtbl.find benchs.by_length length in
  List.fold_left
    (fun best sorter ->
       min best (getter (Hashtbl.find by_length sorter.name)))
    infinity
    !sorters

let build_bench_table benchs getter fmt =
  let heading =
    string_cell "lengths"
    :: string_cell "stable"
    :: list_concat_map (fun length -> [ int_cell length ; string_cell "%" ]) !lengths
  in
  let content =
    !sorters |> List.map @@ fun sorter ->
    string_cell sorter.name
    :: string_cell (if sorter.stable then "yes" else "no")
    :: (
      !lengths |> list_concat_map @@ fun length ->
      let reference = reference benchs getter length in
      let result = getter (get_bench_result ~sorter ~length benchs) in
      let slowdown = Format.sprintf "%.0f" (slowdown ~from:reference ~to_:result) in
      [ fmt_cell (fun () -> result) ~fmt ;
        string_cell (if slowdown = "0" then "" else Format.sprintf "(%s%%)" slowdown) ]
    )
  in
  (heading, content)

let cell_styles =
  ("", Left)
  :: (" | ", Right)
  :: (List.map (fun _ -> [ (" | ", Right) ; (" ", Right) ]) !lengths
      |> List.flatten)

let print_bench_table title benchs getter fmt =
  let (heading, content) = build_bench_table benchs getter fmt in
  print_table ~title ~cell_styles ~heading content

let () = Format.printf "Computing benchmarks on uniformly chosen arrays...@."
let benchs_unif = compute_benchs ~generator:gen_unif
let () = Format.printf "@."

let () = Format.printf "Computing benchmarks on arrays with 5 runs...@."
let benchs_5runs = compute_benchs ~generator:(gen_k_runs 5)
let () = Format.printf "@."

let () = print_bench_table "Unif - Runtime" benchs_unif (fun result -> result.runtime) "%.2f"
let () = Format.printf "@."

let () = print_bench_table "Unif - Comparisons" benchs_unif (fun result -> result.comparisons) "%.0f"
let () = Format.printf "@."

let () = print_bench_table "5-Runs - Runtime" benchs_5runs (fun result -> result.runtime) "%.2f"
let () = Format.printf "@."

let () = print_bench_table "5-Runs - Comparisons" benchs_5runs (fun result -> result.comparisons) "%.0f"
let () = Format.printf "@."
