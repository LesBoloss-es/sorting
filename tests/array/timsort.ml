open Sorting_array

let gen_unif len =
  Array.init len (fun _ -> Random.bits ())

let gen_run ~asc t ofs len =
  let (+-), limit =
    if asc
    then (+), 0
    else (-), Int.max_int
  in

  let rec fill i prev =
    if i >= len then ()
    else begin
      let x = prev +- Random.int 10 in
      t.(ofs + i) <- x;
      fill (i + 1) x
    end
  in
  fill 0 (limit +- Random.int 10)

let gen_k_runs k len =
  let small_len = len / k in
  let t = Array.make len 0 in
  let rec gen_kth k ofs =
    let asc = Random.bool () in
    if k = 0 then assert false
    else if k = 1 then
      gen_run ~asc t ofs (len - ofs)
    else begin
      gen_run ~asc t ofs small_len;
      gen_kth (k - 1) (ofs + small_len)
    end
  in
  gen_kth k 0;
  t

let rec test gen ~nb ~len =
  if nb <= 0 then ()
  else begin
    let t = gen len in
    let t' = Array.copy t in
    Array.stable_sort Int.compare t';
    Timsort.timsort Int.compare t;
    assert (t = t');
    test gen ~nb:(nb - 1) ~len
  end

let () =
  let nb = 100 in
  let len = 1000 in
  test gen_unif ~nb ~len;
  test (gen_k_runs 5) ~nb ~len
