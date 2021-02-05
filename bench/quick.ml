open Sorting.Timsort

let gen_run acc n =
  let rec aux acc prev n =
    if n <= 0 then acc
    else
      let x = Random.int 10 + prev in
      aux (x :: acc) x (n - 1)
  in
  let x = Random.int 3 in
  aux (x :: acc) x (n - 1)


let gen_unif n =
  List.init n (fun _ -> Random.bits ())


let bench data_gen sort1 sort2 n =
  let data = List.init 10 (fun _ -> data_gen n) in
  let rec sort_all sort = function
    | [] -> ()
    | xs :: xss ->
      let _ = Sys.opaque_identity (sort Int.compare xs) in
      sort_all sort xss
  in
  let t = Unix.gettimeofday () in
  let () = sort_all sort1 data in
  let t1 = Unix.gettimeofday () -. t in
  let t = Unix.gettimeofday () in
  let () = sort_all sort2 data in
  let t2 = Unix.gettimeofday () -. t in
  t1, t2

let bench_unif = bench gen_unif
let bench_3run = bench
  (fun n ->
    let n' = n / 3 in
    gen_run (gen_run (gen_run [] n') n') (n - 2 * n'))

let () =
  let n = 20_000 in
  let t1u, t2u = bench_unif List.fast_sort timsort n in
  let t1r, t2r = bench_3run List.fast_sort timsort n in
  Format.printf "List.fast_sort:@.";
  Format.printf "  unif: %F@." t1u;
  Format.printf "  3run: %F@." t1r;
  Format.printf "TimSort:@.";
  Format.printf "  unif: %F@." t2u;
  Format.printf "  3run: %F@." t2r
