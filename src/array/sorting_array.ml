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

    { name = "peeksort" ;
      stable = true ;
      sorter = NearlyOptimalMergesort.PeekSort.sort } ;

    { name = "powersort" ;
      stable = true ;
      sorter = NearlyOptimalMergesort.PowerSort.sort } ;
  ]
