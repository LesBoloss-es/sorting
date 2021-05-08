(** {1 Top-Down Mergesort}

   This module contains the implementation of Top-Down Mergesort implemented as
   part of the experiments in (Munro & Wild 2018) and made available by the
   authors at the address:

   https://github.com/sebawild/nearly-optimal-mergesort-code/blob/master/src/wildinter/net/mergesort/TopDownMergesort.java
   *)

open MergesAndRuns
type 'a cmp = 'a -> 'a -> int

let insertionsortThreshold = 24

let rec mergesort (cmp: 'a cmp) (a: 'a array) (left: int) (right: int) (buffer: 'a array) =
  let n = right - left + 1 in
	if n <= insertionsortThreshold then
	  Insertionsort.insertionsort cmp a left right
  else
    (
	    let m = left + (n lsr 1) in
      mergesort cmp a left (m-1) buffer;
      mergesort cmp a m right buffer;
      mergeRuns cmp a left m right buffer;
    )

(** This function is not given like this in the Java implementation but is here
    for interoperability with the OCaml way of presenting sorting algorithms. *)
let sort (cmp: 'a cmp) (a: 'a array) =
  if a <> [||] then
    let buffer = Array.make (Array.length a) a.(0) in
    mergesort cmp a 0 (Array.length a - 1) buffer
