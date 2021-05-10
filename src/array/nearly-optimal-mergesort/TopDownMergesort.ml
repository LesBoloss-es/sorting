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

let rec mergesortCheckSorted (cmp: 'a cmp) (a: 'a array) (left: int) (right: int) (buffer: 'a array) =
  let n = right - left + 1 in
  if n <= insertionsortThreshold then
    Insertionsort.insertionsort cmp a left right
  else
    (
      let m = left + (n lsr 1) in
      mergesortCheckSorted cmp a left (m-1) buffer;
      mergesortCheckSorted cmp a m right buffer;
      if cmp a.(m-1) a.(m) > 0 then
        mergeRuns cmp a left m right buffer
    )
