(** {1 Sorting functions for arrays} *)

module NearlyOptimalMergesort = NearlyOptimalMergesort

module Timsort = Timsort

(** {2 Ocaml-y Shorthand for Sorting Functions} *)

let stdsort_fast = Array.fast_sort
let stdsort_stable = Array.stable_sort

let timsort = Timsort.timsort

module Nom = struct
  let peeksort cmp a =
    if a <> [||] then
      NearlyOptimalMergesort.PeekSort.peeksort cmp a 0 (Array.length a - 1)

  let peeksort_only_increasing cmp a =
    if a <> [||] then
      NearlyOptimalMergesort.PeekSort.peeksortOnlyIncreasing cmp a 0 (Array.length a - 1)

  let powersort cmp a =
    if a <> [||] then
      NearlyOptimalMergesort.PowerSort.powersort cmp a 0 (Array.length a - 1)

  let timsort cmp a =
    (* Note: Contrary to most other functions of this article, the right bound
       is exclusive in this function. *)
    if a <> [||] then
      NearlyOptimalMergesort.Timsort.sort cmp a 0 (Array.length a)

  let bu_mergesort cmp a =
    if a <> [||] then
      NearlyOptimalMergesort.BottomUpMergesort.mergesort cmp a 0 (Array.length a - 1)

  let bu_mergesort_check_sorted cmp a =
    if a <> [||] then
      NearlyOptimalMergesort.BottomUpMergesort.mergesortCheckSorted cmp a 0 (Array.length a - 1)

  let td_mergesort cmp a =
    if a <> [||] then
      let buffer = Array.make (Array.length a) a.(0) in
      NearlyOptimalMergesort.TopDownMergesort.mergesort cmp a 0 (Array.length a - 1) buffer

  let td_mergesort_check_sorted cmp a =
    if a <> [||] then
      let buffer = Array.make (Array.length a) a.(0) in
      NearlyOptimalMergesort.TopDownMergesort.mergesortCheckSorted cmp a 0 (Array.length a - 1) buffer
end

(** {2 List of All Sorting Functions}

    ... and some additional metadata. *)

type sorter =
  { name : string ;
    stable : bool ;
    sorter : 'a. ('a -> 'a -> int) -> 'a array -> unit }

let all_sorters =
  [
    { name = "stdsort (fast)" ;
      stable = false ;
      sorter = stdsort_fast } ;

    { name = "stdsort (stable)" ;
      stable = true ;
      sorter = stdsort_stable } ;

    { name = "timsort" ;
      stable = true ;
      sorter = timsort } ;

    { name = "nom/timsort" ;
      stable = true ;
      sorter = Nom.timsort } ;

    (** FIXME: All the following functions, from the article "Nearly Optimal
        Mergesort" should be stable, except that their reference implementation
        uses a merge function which is not stable. They are therefore marked as
        unstable here. *)

    { name = "nom/peeksort" ;
      stable = false ;
      sorter = Nom.peeksort } ;

    { name = "nom/peeksort (only incr.)" ;
      stable = false ;
      sorter = Nom.peeksort_only_increasing } ;

    { name = "nom/powersort" ;
      stable = false ;
      sorter = Nom.powersort } ;

    { name = "nom/bu-mergesort" ;
      stable = false ;
      sorter = Nom.bu_mergesort } ;

    { name = "nom/bu-mergesort (check sorted)" ;
      stable = false ;
      sorter = Nom.bu_mergesort_check_sorted } ;

    { name = "nom/td-mergesort" ;
      stable = false ;
      sorter = Nom.td_mergesort } ;

    { name = "nom/td-mergesort (check sorted)" ;
      stable = false ;
      sorter = Nom.td_mergesort_check_sorted } ;
  ]
