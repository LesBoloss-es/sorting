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
      let x = prev +- (Random.int 10 + 1) in
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
