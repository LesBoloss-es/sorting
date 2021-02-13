(** Binary insertion sort *)

type 'a cmp = 'a -> 'a -> int


(** Search where to insert x in the sorted slice t[lo: hi + 1].
  Performs O(log(hi - lo)) comparisons. *)
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


(** Sort the slice t[lo: hi + 1] assuming that t[lo: i] is already sorted. *)
let rec sort_from_i cmp t lo hi i =
  (* variant: hi - i *)
  assert (0 <= lo);
  assert (lo < i);
  assert (i <= hi + 1);
  assert (hi < Array.length t);
  (* assert t[lo: i] is sorted *)
  (* ensures t[lo: hi + 1] is sorted *)
  if i > hi then ()
  else begin
    let x = t.(i) in
    let pos = binary_search cmp t x lo (i - 1) in
    Array.blit t pos t (pos + 1) (i - pos);
    t.(pos) <- x;
    sort_from_i cmp t lo hi (i + 1)
  end

let%test _ =
  let t = [|9; 1; 4; 5; 3; 2; 0|] in
  let i = 4 in
  sort_from_i Int.compare t 1 5 i;
  t = [|9; 1; 2; 3; 4; 5; 0|]


(** Sort the slice t[lo: hi + 1]. *)
let sort cmp t lo hi = sort_from_i cmp t lo hi (lo + 1)

let%test _ =
  let t = [|4; 2; 6; 3|] in
  sort Int.compare t 0 3;
  t = [|2; 3; 4; 6|]

let%test _ =
  let t = [|9; 4; 8; 2; 1; 0|] in
  sort Int.compare t 1 4;
  t = [|9; 1; 2; 4; 8; 0|]
