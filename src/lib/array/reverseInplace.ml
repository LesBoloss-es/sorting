let reverse_inplace (t: 'a array) (offset: int) (len: int) =
  assert (0 <= offset);
  assert (len >= 0);
  assert (offset + len <= Array.length t);
  (* ensures: (t after)[offset:offset+len] = (t before)[offset:offset+len:-1] *)
  for i = 0 to (len - 1) / 2 do
    let tmp = t.(offset + i) in
    t.(offset + i) <- t.(offset + len - 1 - i);
    t.(offset + len - 1 - i) <- tmp
  done

let%test _ =
  let t = [|1; 2; 3; 4; 5; 6|] in
  reverse_inplace t 1 3;
  t = [|1; 4; 3; 2; 5; 6|]

let%test _ =
  let t = [|1; 2; 3; 4; 5; 6|] in
  reverse_inplace t 1 4;
  t = [|1; 5; 4; 3; 2; 6|]

let reverse_inplace_range t l r =
  reverse_inplace t l (r - l + 1)
