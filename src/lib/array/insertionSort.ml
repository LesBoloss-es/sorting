(** Binary insertion sort *)

type 'a cmp = 'a -> 'a -> int

let rec binary_search cmp t x lo hi =
  (* assert a[lo: hi + 1] is sorted *)
  assert (0 <= lo);
  assert (lo <= hi + 1); (* lo should generally be <= hi except eventually in
                            the last recursive call where hi might be equal to
                            lo - 1. In this case lo is "correct" and hi is "out
                            of bounds" and must be replaced by hi + 1. *)
  assert (hi < Array.length t);
  (* ensures: (forall i < result.  cmp a.(i) x <= 0)
           /\ (forall i >= result. cmp a.(i) x > 0) *)

  let mid = lo + (hi - lo) / 2 in
  assert (lo >= hi || mid < hi);

  let mid_le_x = cmp t.(mid) x <= 0 in

  if lo >= hi then
    if mid_le_x then lo + 1 else lo
    (* Or the "branching-free": *)
    (* lo + (Obj.magic mid_le_x: int) *)
  else if mid_le_x then
    binary_search cmp t x (mid + 1) hi
  else
    binary_search cmp t x lo (mid - 1)


let sort cmp t lo hi =
  assert (0 <= lo);
  assert (lo <= hi);
  assert (hi < Array.length t);
  (* ensures: t[lo: hi + 1] is sorted *)

  for i = lo + 1 to hi do
    (* invariant: t[lo: i]  is sorted. *)
    let x = t.(i) in
    let pos = binary_search cmp t x lo (i - 1) in
    Array.blit t pos t (pos + 1) (i - pos);
    t.(pos) <- x
  done

let%test _ =
  let t = [|4; 2; 6; 3|] in
  sort Int.compare t 0 3;
  t = [|2; 3; 4; 6|]

let%test _ =
  let t = [|9; 4; 8; 2; 1; 0|] in
  sort Int.compare t 1 4;
  t = [|9; 1; 2; 4; 8; 0|]
