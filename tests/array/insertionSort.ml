open Sorting_array
open Genlib

let rec test gen ~nb ~len =
  if nb <= 0 then ()
  else begin
    let t = gen len in
    let t' = Array.copy t in
    Array.stable_sort Int.compare t';
    InsertionSort.sort Int.compare t 0 (len - 1);
    assert (t = t');
    test gen ~nb:(nb - 1) ~len
  end

let () =
  let nb = 100 in
  let len = 1000 in
  test Genarray.gen_unif ~nb ~len;
  test (Genarray.gen_k_runs 5) ~nb ~len
