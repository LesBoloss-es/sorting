(** {1 Merges and Runs}

   This file is simply an OCaml version of
   https://github.com/sebawild/nearly-optimal-mergesort-code/blob/master/src/wildinter/net/mergesort/Insertionsort.java
   written by Sebastian Wild (wild@uwaterloo.ca).

   The implementations and naming conventions are really not typical of OCaml
   but the goal is to be really close to the implementation of the authors.

   The main differences are the following:

   - all the functions are agnostic and take a comparison function (when
     necessary) as first argument instead of only sorting integers.

   - since OCaml does not have overloading of functions, we had to slightly
     rename some endpoints. This is always indicated in the documentation.

   - since OCaml does not have a [break] mechanism, while loops have been
     rewritten into recursive functions. *)

type 'a cmp = 'a -> 'a -> int

(** Sort A[left..right] by straight-insertion sort (both endpoints inclusive) *)
let insertionsort (cmp: 'a cmp) (a: 'a array) (left: int) (right: int) =
	for i = left + 1 to right do
	  let j = ref (i - 1) in
    let v = a.(i) in
    let rec while_loop () =
      if cmp v a.(!j) < 0 then
        (
          a.(!j+1) <- a.(!j);
          decr j;
          if !j < left then ()
          else while_loop ()
        )
    in
    while_loop ();
    a.(!j+1) <- v
  done

(** Sort A[left..right] by straight-insertion sort (both endpoints inclusive),
   assuming the leftmost nPresorted elements form a weakly increasing run. This
   function is called [insertionsort] in the original Java code. *)
let insertionsort_presorted (cmp: 'a cmp) (a: 'a array) (left: int) (right: int) (nPresorted: int) =
	assert (right >= left);
  assert (right - left + 1 >= nPresorted);
  for i = left + nPresorted to right do
	  let j = ref (i - 1) in
	  let v = a.(i) in
    let rec while_loop () =
      if cmp v a.(!j) < 0 then
        (
          a.(!j+1) <- a.(!j);
          decr j;
          if !j < left then ()
          else while_loop ()
        )
    in
    while_loop ();
    a.(!j+1) <- v
  done

(** Sort A[left..right] by straight-insertion sort (both endpoints inclusive),
   assuming the rightmost nPresorted elements form a weakly increasing run. This
   function is called [insertionsortRight] in the original Java code. *)
let insertionsortRight_presorted (cmp: 'a cmp) (a: 'a array) (left: int) (right: int) (nPresorted: int) =
	assert (right >= left);
  assert (right - left + 1 >= nPresorted);
  for i = right - nPresorted downto left do
	  let j = ref (i + 1) in
	  let v = a.(i) in
    let rec while_loop () =
      if cmp v a.(!j) > 0 then
        (
          a.(!j-1) <- a.(!j);
          incr j;
          if !j > right then ()
          else while_loop ()
        )
    in
    while_loop ();
    a.(!j-1) <- v
  done
