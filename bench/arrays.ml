open Genlib.Genarray
open Table
open Sorting_array

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

let lengths = List.init 5 (fun i -> (i+ 1) * 30000)

let repeat = 50

let () =
  Format.(
    printf "lengths = %a.@."
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt ", ")
         pp_print_int)
      lengths
  )

let () =
  Format.(
    printf "sorters = %a.@."
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt ", ")
         pp_print_string)
      (List.map (fun sorter -> sorter.name) all_sorters)
  )

let () = Format.printf "repeat = %d@." repeat
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
    lengths |> List.iter @@ fun length ->
    Format.printf "[%d]@?" length;
    let inputs = List.init repeat (fun _ -> generator length) in
    (
      all_sorters |> List.iter @@ fun sorter ->
      Format.printf " %s@?" sorter.name;
      let runtime = bench_one ~measure:runtime ~algorithm:sorter.sorter ~inputs () in
      let comparisons = bench_one ~measure:comparisons ~algorithm:sorter.sorter ~inputs () in
      let repeat = float_of_int repeat in
      add_bench ~sorter ~length ~runtime:(runtime /. repeat *. 1000.) ~comparisons:(comparisons /. repeat) results
    );
    Format.printf "@."
  );
  results

let slowdown ~from ~to_ = (to_ -. from) /. from *. 100.

let reference benchs getter length =
  let by_length = Hashtbl.find benchs.by_length length in
  List.fold_left
    (fun best sorter ->
       min best (getter (Hashtbl.find by_length sorter.name)))
    infinity
    all_sorters

let build_bench_table benchs getter fmt =
  (
    string_cell "lengths"
    :: string_cell "stable"
    :: List.concat_map (fun length -> [ int_cell length ; string_cell "%" ]) lengths
  ) :: (
    all_sorters |> List.map @@ fun sorter ->
    string_cell sorter.name
    :: string_cell (if sorter.stable then "yes" else "no")
    :: (
      lengths |> List.concat_map @@ fun length ->
      let reference = reference benchs getter length in
      let result = getter (get_bench_result ~sorter ~length benchs) in
      let slowdown = Format.sprintf "%.0f" (slowdown ~from:reference ~to_:result) in
      [ fmt_cell (fun () -> result) ~fmt ;
        string_cell (if slowdown = "0" then "" else Format.sprintf "(%s%%)" slowdown) ]
    )
  )

let cell_styles =
  (Auto (ref 14), Left)
  :: (Auto (ref 6), Right)
  :: (List.map (fun _ -> [ (Auto (ref 6), Right) ; (Auto (ref 5), Right) ]) lengths
      |> List.flatten)

let print_bench_table title benchs getter fmt =
  print_table ~title ~cell_styles (build_bench_table benchs getter fmt)

let () = Format.printf "Computing benchmarks on uniformly chosen arrays...@."
let benchs_unif = compute_benchs ~generator:gen_unif
let () = Format.printf "@."

let () = Format.printf "Computing benchmarks on arrays with 5 runs...@."
let benchs_5runs = compute_benchs ~generator:(gen_k_runs 5)
let () = Format.printf "@."

let () = print_bench_table "Unif -- Runtime" benchs_unif (fun result -> result.runtime) "%.2f"
let () = Format.printf "@."

let () = print_bench_table "Unif -- Comparisons" benchs_unif (fun result -> result.comparisons) "%.0f"
let () = Format.printf "@."

let () = print_bench_table "5-Runs -- Runtime" benchs_5runs (fun result -> result.runtime) "%.2f"
let () = Format.printf "@."

let () = print_bench_table "5-Runs -- Comparisons" benchs_5runs (fun result -> result.comparisons) "%.0f"
let () = Format.printf "@."
