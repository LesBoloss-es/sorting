open Genlib.Genarray
open Allfuns.Allarrays

let test_gen ~prep ~cmp ~sort ~gen ~nb ~len =
  let rec aux = function
    | 0 -> ()
    | n ->
      let t = prep (gen len) in
      let t' = Array.copy t in
      Array.stable_sort cmp t';
      sort cmp t;
      if t <> t' then failwith "test_gen";
      aux (n-1)
  in
  aux nb

let test ~sort ~gen ~nb ~len =
  test_gen ~prep:Fun.id ~cmp:Int.compare ~sort ~gen ~nb ~len

let test_stable ~sort ~gen ~nb ~len =
  let prep = Array.mapi (fun i x -> (i, x)) in
  let cmp = fun (_, x) (_, y) -> Int.compare x y in
  test_gen ~prep ~cmp ~sort ~gen ~nb ~len

let failure = ref false

let test_one (name, sort) =
  Format.printf "Checking that %s sorts correctly... @?" name;
  try
    let nb = 100 in
    Format.printf "[0] @?";
    test ~sort ~gen:gen_unif ~nb ~len:0;
    for log2_len = 0 to 5 do
      let len = 1 lsl (3 * log2_len) in
      Format.printf "[%d] @?" len;
      test ~sort ~gen:gen_unif ~nb ~len;
      test ~sort ~gen:(gen_k_runs 5) ~nb ~len
    done;
    Format.printf "done.@."
  with
    Failure _ ->
    Format.printf "it does NOT!@.";
    failure := true

let test_one_stable (name, sort) =
  Format.printf "Checking that %s sorts in a stable way... @?" name;
  try
    let nb = 20 in
    for log2_len = 1 to 5 do
      let len = 1 lsl (3 * log2_len) in
      Format.printf "[%d] @?" len;
      test_stable ~sort ~gen:gen_unif ~nb ~len;
      test_stable ~sort ~gen:(gen_k_runs 5) ~nb ~len
    done;
    Format.printf "done.@."
  with
    Failure _ ->
    Format.printf "it does NOT!@.";
    failure := true

let () = List.iter test_one allarraysorts
let () = List.iter test_one_stable allarraysorts
