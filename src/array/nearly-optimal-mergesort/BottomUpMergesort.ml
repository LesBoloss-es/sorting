(** {1 Bottom-Up Mergesort}

   This module contains the implementation of Bottom-Up Mergesort implemented as
   part of the experiments in (Munro & Wild 2018) and made available by the
   authors at the address:

   https://github.com/sebawild/nearly-optimal-mergesort-code/blob/master/src/wildinter/net/mergesort/BottomUpMergesort.java
   *)

open MergesAndRuns
type 'a cmp = 'a -> 'a -> int

let minRunLen = 24

let mergesort (cmp: 'a cmp) (a: 'a array) (left: int) (right: int) =
  let n = right - left + 1 in
  if a <> [||] then
    (
      let b = Array.make n a.(0) in
      if minRunLen <> 1 then
        (
			    (* for (int len = minRunLen, i = left; i <= right; i += len) *)
          let len = minRunLen in
          let i = ref left in
          while !i <= right do
			      Insertionsort.insertionsort cmp a !i (min (!i+len-1) right);
            i := !i + len
          done
        );
	    (* for (int len = minRunLen; len < n; len *= 2) *)
      let len = ref minRunLen in
      while !len < n do
		    (* for (int i = left; i <= right - len; i += len + len) *)
        let i = ref left in
        while !i <= right - !len do
			    mergeRuns cmp a !i (!i + !len) (min (!i + !len + !len - 1) right) b;
          i := !i + !len + !len
        done;
        len := !len * 2
      done
    )

let mergesortCheckSorted (cmp: 'a cmp) (a: 'a array) (left: int) (right: int) =
  let n = right - left + 1 in
  let b = Array.make (Array.length a) a.(0) in
  if minRunLen <> 1 then
    (
      let len = minRunLen in
      let i = ref left in
      while !i <= right do
        Insertionsort.insertionsort cmp a !i (min (!i + len - 1) right);
        i := !i + len
      done
    );
  let len = ref minRunLen in
  while !len < n do
    let i = ref left in
    while !i <= right - !len do
      if cmp a.(!i + !len - 1) a.(!i + !len) > 0 then
        mergeRuns cmp a !i (!i + !len) (min (!i + !len + !len - 1) right) b;
      i := !i + !len + !len
    done;
    len := !len * 2
  done
