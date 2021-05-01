(** {1 Merges and Runs}

   This file is simply an OCaml version of
   https://github.com/sebawild/nearly-optimal-mergesort-code/blob/master/src/wildinter/net/mergesort/MergesAndRuns.java
   written by Sebastian Wild (wild@uwaterloo.ca).

   The implementations and naming conventions are really not typical of OCaml
   but the goal is to be really close to the implementation of the authors.

   The main difference is that all the functions are agnostic and take a
   comparison function (when necessary) as first argument instead of only
   sorting integers. *)

type 'a cmp = 'a -> 'a -> int

(** Merges runs A[l..m-1] and A[m..r] in-place into A[l..r] with Sedgewick's
   bitonic merge (Program 8.2 in Algorithms in C++) using B as temporary
   storage. B.length must be at least r+1. *)
let mergeRuns (cmp: 'a cmp) (a: 'a array) (l: int) (m: int) (r: int) (b: 'a array) =
  let m = m - 1 in (* mismatch in convention with Sedgewick *)
  assert (Array.length b >= r+1);
  for i = m+1 downto l+1 do
    b.(i-1) <- a.(i-1)
  done;
  for j = m to r-1 do
    b.(r+m-j) <- a.(j+1)
  done;
  let i = ref l in
  let j = ref r in
  for k = l to r do
    a.(k) <-
      if cmp b.(!j) b.(!i) < 0 then
        (let bj = b.(!j) in decr j; bj)
      else
        (let bi = b.(!i) in incr i; bi)
  done

(** Reverse the specified range of the specified array.

	  @param a  the array in which a range is to be reversed
	  @param lo the index of the first element in the range to be reversed
	  @param hi the index of the last element in the range to be reversed *)
let reverseRange (a: 'a array) (lo: int) (hi: int) =
  let lo = ref lo in
  let hi = ref hi in
  while !lo < !hi do
    let t = a.(!lo) in
    a.(!lo) <- a.(!hi);
    a.(!hi) <- t;
    incr lo;
    decr hi
  done

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
