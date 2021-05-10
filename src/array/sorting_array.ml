module NearlyOptimalMergesort = NearlyOptimalMergesort

module Timsort = Timsort

type sorter =
  { name : string ;
    stable : bool ;
    sorter : 'a. ('a -> 'a -> int) -> 'a array -> unit }

let all_sorters =
  [
    { name = "stdlib(fast)" ;
      stable = false ;
      sorter = Array.fast_sort } ;

    { name = "stdlib(stable)" ;
      stable = true ;
      sorter = Array.stable_sort } ;

    { name = "timsort" ;
      stable = true ;
      sorter = Timsort.timsort } ;

    (** FIXME: PeekSort should be stable, but this implementation uses a merge
       function which is not stable! *)
    { name = "nom/peeksort" ;
      stable = false ;
      sorter = NearlyOptimalMergesort.PeekSort.sort } ;

    (** FIXME: PowerSort should be stable, but this implementation uses a merge
       function which is not stable! *)
    { name = "nom/powersort" ;
      stable = false ;
      sorter = NearlyOptimalMergesort.PowerSort.sort } ;

    { name = "nom/timsort" ;
      stable = true ;
      sorter = NearlyOptimalMergesort.Timsort.sort } ;

    (** FIXME: Bottom-Up Mergesort should be stable, but this implementation
       uses a merge function which is not stable! *)
    { name = "nom/bu-mergesort" ;
      stable = false ;
      sorter = NearlyOptimalMergesort.BottomUpMergesort.sort } ;

    (** FIXME: Top-Down Mergesort should be stable, but this implementation
        uses a merge function which is not stable! *)
    { name = "nom/td-mergesort" ;
      stable = false ;
      sorter = NearlyOptimalMergesort.TopDownMergesort.sort } ;
  ]
