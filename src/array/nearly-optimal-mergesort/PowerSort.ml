(** {1 PowerSort}

    This module contains the implementation of PowerSort described in (Munro &
    Wild 2018) and implemented by the authors at the address:

    https://github.com/sebawild/nearly-optimal-mergesort-code/blob/master/src/wildinter/net/mergesort/PowerSort.java *)

open MergesAndRuns
type 'a cmp = 'a -> 'a -> int

let minRunLen = 16

let null_index = Int.min_int

let nodePower (left: int) (right: int) (startA: int) (startB: int) (endB: int) =
	let n = right - left + 1 in
	let l = startA + startB - (left lsl 1) in (* 2*middleA *)
  let r = startB + endB + 1 - (left lsl 1) in (* 2*middleB *)
  let a = ((l lsl 30) / n) in (* middleA / 2n *)
  let b = ((r lsl 30) / n) in (* middleB / 2n *)
  Base.Int.(clz (pow a b))

let powersort (cmp: 'a cmp) (a: 'a array) (left: int) (right: int) =
	let n = right - left + 1 in
  let lgnPlus2 = Base.Int.floor_log2 n + 2 in
  let leftRunStart = Array.make lgnPlus2 null_index in
  let leftRunEnd   = Array.make lgnPlus2 0 in
 	let top = ref 0 in
	let buffer = Array.make n a.(0) in
  let startA = ref left in
  let endA = ref (extendAndReverseRunRight cmp a !startA right) in
	(* extend to minRunLen *)
	let lenA = !endA - !startA + 1 in
	if lenA < minRunLen then
    (
			endA := min right (!startA + minRunLen-1);
			Insertionsort.insertionsort_presorted cmp a !startA !endA lenA
    );
	while !endA < right do
		let startB = !endA + 1 in
    let endB = ref (extendAndReverseRunRight cmp a startB right) in
	  (* extend to minRunLen *)
    let lenB = !endB - startB + 1 in
	  if lenB < minRunLen then
      (
				endB := min right (startB + minRunLen-1);
				Insertionsort.insertionsort_presorted cmp a startB !endB lenB
      );
		let k = nodePower left right !startA startB !endB in
	  assert (k <> !top);
    (* for (int l = top; l > k; --l) *)
    let rec for_loop l =
      if l <= k then ()
      else
        (
          if leftRunStart.(l) = null_index then for_loop (l-1)
          else
            (
              mergeRuns cmp a leftRunStart.(l) (leftRunEnd.(l)+1) !endA buffer;
              startA := leftRunStart.(l);
              leftRunStart.(l) <- null_index;
              for_loop (l-1)
            )
        )
    in
    for_loop !top;
    (* store left half of merge between A and B *)
	  leftRunStart.(k) <- !startA;
    leftRunEnd.(k) <- !endA;
	  top := k;
	  startA := startB;
    endA := !endB;
	done;
	assert (!endA = right);
  (* for (int l = top; l > 0; --l) *)
  let rec for_loop l =
	  if leftRunStart.(l) = null_index then for_loop (l-1)
    else
      (
        mergeRuns cmp a leftRunStart.(l) (leftRunEnd.(l)+1) right buffer;
        for_loop (l-1)
      )
  in
  for_loop !top

(** This function is not given like this in the Java implementation but is here
   for interoperability with the OCaml way of presenting sorting algorithms. *)
let sort (cmp: 'a cmp) (a: 'a array) =
  powersort cmp a 0 (Array.length a - 1)
