(** {1 PowerSort}

    This module contains the implementation of PowerSort described in (Munro &
    Wild 2018) and implemented by the authors at the address:

    https://github.com/sebawild/nearly-optimal-mergesort-code/blob/master/src/wildinter/net/mergesort/PowerSort.java *)

open MergesAndRuns
type 'a cmp = 'a -> 'a -> int

let minRunLen = 16

(* FIXME: nodePower, which should be the best implementation, is completely
   buggy and does not pass the associated tests. In the rest of the code, we
   will replace it by one of the other nodePower* but this will slow down the
   algorithm. *)

let nodePower (left: int) (right: int) (startA: int) (startB: int) (endB: int) =
	let n = right - left + 1 in
	let l = startA + startB - (left lsl 1) in (* 2*middleA *)
  let r = startB + endB + 1 - (left lsl 1) in (* 2*middleB *)
  let a = (l lsl 30) / n in (* middleA / 2n *)
 let b = (r lsl 30) / n in (* middleB / 2n *)
 (* Java's integers are 32 bits, and their number of leading zeros is defined
    according to this. We substract 31 to the result here to go from 63 bits to
    32. We could substract (Sys.int_size - 32) for genericity but there is not
    guarantee that the rest of this function wouldn't overflow on 31 bits. So we
    play it safe and add an assertion here. *)
 assert (Sys.int_size = 63);
 Base.Int.(clz (a lxor b)) - 31

let%test _ = nodePower 1 100 10 20 25 = 4
let%test _ = nodePower 0 21 8 12 13 = 1
let%test _ = nodePower 0 21 19 20 20 = 5
let%test _ = nodePower 0 (100*1000*1000) 55555555 55555666 55556666 = 16

let nodePowerBitwise (left: int) (right: int) (startA: int) (startB: int) (endB: int) =
	assert (right < (1 lsl 30)); (* otherwise nt2, l and r will overflow *)
  let n = right - left + 1 in
  let l = ref (startA - (left lsl 1) + startB) in
  let r = ref (startB - (left lsl 1) + endB + 1) in
  (* a and b are given by l/nt2 and r/nt2, both are in [0,1).
	   we have to find the number of common digits in the
     binary representation in the fractional part. *)
  let nCommonBits = ref 0 in
  let digitA = ref (!l >= n) in
  let digitB = ref (!r >= n) in
  while !digitA = !digitB do
    incr nCommonBits;
    l := !l - (if !digitA then n else 0);
    r := !r - (if !digitA then n else 0);
    l := !l lsl 1;
    r := !r lsl 1;
    digitA := !l >= n;
    digitB := !r >= n
  done;
  !nCommonBits + 1

let%test _ = nodePowerBitwise 1 100 10 20 25 = 4
let%test _ = nodePowerBitwise 0 21 8 12 13 = 1
let%test _ = nodePowerBitwise 0 21 19 20 20 = 5
let%test _ = nodePowerBitwise 0 (100*1000*1000) 55555555 55555666 55556666 = 16

let nodePowerLoop (left: int) (right: int) (startA: int) (startB: int) (endB: int) =
  assert (right < (1 lsl 30)); (* otherwise nt2 will overflow *)
	let nt2 = (right - left + 1) lsl 1 in (* 2*n *)
  let n1 = startB - startA in
  let n2 = endB - startB + 1 in
  let a = ref ((startA lsl 1) + n1 - (left lsl 1)) in
  let b = ref ((startB lsl 1) + n2 - (left lsl 1)) in
  let k = ref 0 in
  while !b - !a <= nt2 && !a / nt2 = !b / nt2 do
	  incr k;
	  a := !a lsl 1;
    b := !b lsl 1
  done;
  !k

let%test _ = nodePowerLoop 1 100 10 20 25 = 4
let%test _ = nodePowerLoop 0 21 8 12 13 = 1
let%test _ = nodePowerLoop 0 21 19 20 20 = 5
let%test _ = nodePowerLoop 0 (100*1000*1000) 55555555 55555666 55556666 = 16

let null_index = Int.min_int

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
    if l <= 0 then ()
    else
      (
	      if leftRunStart.(l) = null_index then for_loop (l-1)
        else
          (
            mergeRuns cmp a leftRunStart.(l) (leftRunEnd.(l)+1) right buffer;
            for_loop (l-1)
          )
      )
  in
  for_loop !top

let powersort (cmp: 'a cmp) (a: 'a array) (left: int) (right: int) =
  if a = [||] then ()
  else powersort cmp a left right

(** This function is not given like this in the Java implementation but is here
    for interoperability with the OCaml way of presenting sorting algorithms. *)
let sort (cmp: 'a cmp) (a: 'a array) =
  powersort cmp a 0 (Array.length a - 1)
