(** {1 PeekSort}

    This module contains the implementation of Peeksort described in (Munro &
    Wild 2018) and implemented by the authors at the address:

    https://github.com/sebawild/nearly-optimal-mergesort-code/blob/master/src/wildinter/net/mergesort/PeekSort.java *)

open MergesAndRuns
type 'a cmp = 'a -> 'a -> int

let insertion_sort_threshold = 10

let rec peeksort (cmp: 'a cmp) (a: 'a array) (left: int) (right: int) (leftRunEnd: int) (rightRunStart: int) (b: 'a array) =
  if leftRunEnd = right || rightRunStart = left then
    ()
  else if right - left + 1 <= insertion_sort_threshold then
    Insertionsort.insertionsort_presorted cmp a left right (leftRunEnd - left + 1)
  else
    (
      let mid = left + ((right - left) lsr 1) in (* left + ((right - left) / 2) *)
      if mid <= leftRunEnd then
        (
          peeksort cmp a (leftRunEnd+1) right (leftRunEnd+1) rightRunStart b;
          mergeRuns cmp a left (leftRunEnd+1) right b
        )
      else if mid >= rightRunStart then
        (
          peeksort cmp a left (rightRunStart-1) leftRunEnd (rightRunStart-1) b;
          mergeRuns cmp a left rightRunStart right b
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
                reverseRange a i j;
                (i, j)
              )
          in
          if i = left && j = right then
            ()
          else
            (
              if mid - i < j - mid then
                (
                  peeksort cmp a left (i-1) leftRunEnd (i-1) b;
                  peeksort cmp a i right j rightRunStart b;
                  mergeRuns cmp a left i right b
                )
              else
                (
                  peeksort cmp a left j leftRunEnd i b;
                  peeksort cmp a (j+1) right (j+1) rightRunStart b;
                  mergeRuns cmp a left (j+1) right b
                )
            )
        )
    )

let peeksort (cmp: 'a cmp) (a: 'a array) (l: int) (r: int) =
  if a != [||] then
    (
      let b = Array.make (r - l + 1) a.(0) in
      peeksort cmp a l r l r b
    )

(** This function is not given like this in the Java implementation but is here
   for interoperability with the OCaml way of presenting sorting algorithms. *)
let sort (cmp: 'a cmp) (a: 'a array) =
  peeksort cmp a 0 (Array.length a - 1)
