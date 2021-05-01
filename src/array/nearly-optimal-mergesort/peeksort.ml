(** {1 Peeksort}

    This module contains the implementation of Peeksort described in (Munro &
    Wild 2018). *)

(* The naming conventions for this algorithm are the same as that of Timsort: a
   "run" is either a maximal weakly increasing or maximal strictly decreasing
   region. The fact that we consider only strictly decreasing runs allows to
   reverse them on detection without loosing the stability of the algorithm. *)

(* For reference, here is a verbatim copy of "Algorithm 1"
   from (Munro & Wild 2018).

   PeekSort(A[l..r], e, s)

    1  if e == r or s == l then return
    2  m := l + floor((r-l) / 2)
    3  if m <= e
    4    PeekSort(A[e+1..r], e+1, s)
    5    Merge(A[l..e], A[e+1..r])
    6  else if m >= s
    7    PeekSort(A[l..s-1], e, s-1)
    8    Merge(A[l..s-1], A[s..r])
    9  else
   10    i := ExtendRunLeft(A[m], l);  j := ExtendRunRight(A[m], r);
   11    if i == l and j == r return
   12    if m - i < j - m
   13      PeekSort(A[l..i-1], e, i-1)
   14      PeekSort(A[i..r], j, s)
   15      Merge(A[l..i-1], A[i..r])
   16    else
   17      PeekSort(A[l..j], e, i)
   18      PeekSort(A[j+1..r], j+1, s)
   19      Merge(A[l..j], A[j+1..r]) *)

(* Note: The algorithm in the article is quite imprecise. It is better to see
   the version that has been implemented by the authors at the address:

    https://github.com/sebawild/nearly-optimal-mergesort-code/blob/master/src/wildinter/net/mergesort/PeekSort.java *)

type 'a cmp = 'a -> 'a -> int

let valid_slice a l r = 0 <= l && l <= r && r < Array.length a

let rec extend_increasing_run_left cmp a l r =
  (* extend an increasing run that ends on index r *)
  if r <= l || cmp a.(r-1) a.(r) > 0 then r
  else extend_increasing_run_left cmp a l (r-1)

let rec extend_decreasing_run_left cmp a l r =
  (* extend a decreasing run that ends on index r *)
  if r <= l || cmp a.(r-1) a.(r) <= 0 then r
  else extend_decreasing_run_left cmp a l (r-1)

let rec extend_increasing_run_right cmp a l r =
  (* extend an increasing run that starts on index l *)
  if r <= l || cmp a.(l) a.(l+1) > 0 then l
  else extend_increasing_run_right cmp a (l+1) r

let rec extend_decreasing_run_right cmp a l r =
  (* extend a decreasing run that starts on index l *)
  if r <= l || cmp a.(l) a.(l+1) <= 0 then l
  else extend_decreasing_run_right cmp a (l+1) r

let extend_run cmp a l m r =
  let cl = cmp a.(m-1) a.(m) in
  let cr = cmp a.(m) a.(m+1) in
  if cl <= 0 then
    if cr <= 0 then
      (* on the left, we are at the end of a weakly increasing run *)
      (* on the right, we are at the beginning of a weakly increasing run *)
      (* we can just merge both runs together *)
      let i = extend_increasing_run_left cmp a l (m-1) in
      let j = extend_increasing_run_right cmp a (m+1) r in
      (i, j)
    else
      (* on the left, we are at the end of a weakly increasing run *)
      (* on the right, we are at the beginning of a strictly decreasing run *)
      (* we take the longest of the two *)
      let i = extend_increasing_run_left cmp a l (m-1) in
      let j = extend_decreasing_run_right cmp a (m+1) r in
      if m - i < j - m then
        (m, j)
      else
        (i, m)
  else
  if cr <= 0 then
    (* on the left, we are at the end of a strictly decreasing run *)
    (* on the right, we are at the beginning of a weakly increasing run *)
    (* we take the longest of the two *)
    let i = extend_decreasing_run_left cmp a l (m-1) in
    let j = extend_increasing_run_right cmp a (m+1) r in
    if m - i < j - m then
      (m, j)
    else
      (i, m)
  else
    (* on the left, we are at the end of a strictly decreasing run *)
    (* on the right, we are at the beginning of a strictly decreasing run *)
    (* we can just merge both runs together *)
    let i = extend_decreasing_run_left cmp a l (m-1) in
    let j = extend_decreasing_run_right cmp a (m+1) r in
    (i, j)

let extend_run cmp a l m r =
  assert (0 <= l && l <= m && m <= r && r < Array.length a);
  let (i, j) = extend_run cmp a l m r in
  assert (l <= i && i <= m && m <= j && j <= r);
  (i, j)

let merge (cmp: 'a cmp) (a: 'a array) (b: 'a array) (l: int) (m: int) (m': int) (r: int) =
  assert (valid_slice a l m);
  assert (valid_slice a m' r);
  assert (m + 1 = m');

  let len_lo = m - l  + 1 in
  let len_hi = r - m' + 1 in

  if len_lo < len_hi then
    (
      Array.blit a l b 0 len_lo;
      Timsort.merge_lo cmp a l b 0 len_lo a m' len_hi
    )
  else
    (
      Array.blit a m' b 0 len_hi;
      Timsort.merge_hi cmp a l a l len_lo b 0 len_hi
    )

let merge cmp a b l m r =
  merge cmp a b l (m-1) m r

let extendWeaklyIncreasingRunLeft (cmp: 'a cmp) (a: 'a array) (i: int) (left: int) =
  let i = ref i in
  while !i > left && cmp a.(!i-1) a.(!i) <= 0 do
    decr i
  done;
  !i

let extendWeaklyIncreasingRunRight (cmp: 'a cmp) (a: 'a array) (i: int) (right: int) =
  let i = ref i in
	while !i < right && cmp a.(!i+1) a.(!i) >= 0 do
   incr i
 done;
 !i

let extendStrictlyDecreasingRunLeft (cmp: 'a cmp) (a: 'a array) (i: int) (left: int) =
  let i = ref i in
	while !i > left && cmp a.(!i-1) a.(!i) > 0 do
   decr i
 done;
 !i

let extendStrictlyDecreasingRunRight (cmp: 'a cmp) (a: 'a array) (i: int) (right: int) =
  let i = ref i in
	while !i < right && cmp a.(!i+1) a.(!i) < 0 do
   incr i
 done;
 !i

let insertion_sort_threshold = -1

let rec peeksort (cmp: 'a cmp) (a: 'a array) (b: 'a array) (left: int) (right: int) (leftRunEnd: int) (rightRunStart: int) =
  if leftRunEnd = right || rightRunStart = left then
    ()
  else if right - left + 1 <= insertion_sort_threshold then
    InsertionSort.insertion_sort cmp a left right (leftRunEnd - left + 1)
  else
    (
      let mid = left + ((right - left) lsr 1) in (* left + ((right - left) / 2) *)
      if mid <= leftRunEnd then
        (
          peeksort cmp a b (leftRunEnd+1) right (leftRunEnd+1) rightRunStart;
          merge cmp a b left (leftRunEnd+1) right
        )
      else if mid >= rightRunStart then
        (
          peeksort cmp a b left (rightRunStart-1) leftRunEnd (rightRunStart-1);
          merge cmp a b left rightRunStart right
        )
      else
        (
          let (i, j) =
            if cmp a.(mid) a.(mid+1) <= 0 then
              (
                let i = extendWeaklyIncreasingRunLeft cmp a mid (if left = leftRunEnd then left else leftRunEnd+1) in
                let j =
                  if mid+1 = rightRunStart then mid
                  else extendWeaklyIncreasingRunRight cmp a (mid+1) (if right = rightRunStart then right else rightRunStart-1)
                in
                (i, j)
              )
            else
              (
                let i = extendStrictlyDecreasingRunLeft cmp a mid (if left = leftRunEnd then left else leftRunEnd+1) in
                let j =
                  if mid+1 = rightRunStart then mid
                  else extendStrictlyDecreasingRunRight cmp a (mid+1) (if right = rightRunStart then right else rightRunStart-1)
                in
                ReverseInplace.reverse_inplace_range a i j;
                (i, j)
              )
          in
          if i = left && j = right then
            ()
          else
            (
              if mid - i < j - mid then
                (
                  peeksort cmp a b left (i-1) leftRunEnd (i-1);
                  peeksort cmp a b i right j rightRunStart;
                  merge cmp a b left i right
                )
              else
                (
                  peeksort cmp a b left j leftRunEnd i;
                  peeksort cmp a b (j+1) right (j+1) rightRunStart;
                  merge cmp a b left (j+1) right
                )
            )
        )
    )

let peeksort (cmp: 'a cmp) (a: 'a array) =
  if a != [||] then
    (
      let b = Array.make (Array.length a / 2) a.(0) in
      let z = Array.length a - 1 in
      peeksort cmp a b 0 z 0 z
    )

let%test _ =
  let t = [|0;6;2;6;4;1;9;7|] in
  let t_sorted = Array.copy t in
  Array.sort Int.compare t_sorted;
  peeksort Int.compare t;
  t = t_sorted

let%test _ =
  let t = Array.mapi (fun i x -> i, x) [|0;6;2;6;0;1;0;2|] in
  let t_sorted = Array.copy t in
  let cmp (_, x) (_, y) = Int.compare x y in
  Array.stable_sort cmp t_sorted;
  peeksort cmp t;
  t = t_sorted

let%test _ =
  peeksort compare [||]; true

let%test _ =
  let t = [|true|] in
  peeksort Bool.compare t;
  t.(0)
