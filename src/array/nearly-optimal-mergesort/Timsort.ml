type 'a cmp = 'a -> 'a -> int

(** This is the minimum sized sequence that will be merged. Shorter sequences
    will be lengthened by calling binarySort. If the entire array is less than
    this length, no merges will be performed.

    This constant should be a power of two. It was 64 in Tim Peter's C
    implementation, but 32 was empirically determined to work better in this
    implementation. In the unlikely event that you set this constant to be a
    number that's not a power of two, you'll need to change the {@link
    #minRunLength} computation.

    If you decrease this constant, you must change the stackLen computation in
    the TimSort constructor, or you risk an ArrayOutOfBounds exception. See
    listsort.txt for a discussion of the minimum stack length required as a
    function of the length of the array being sorted and the minimum merge
    sequence length. *)

let min_merge = 32

(** When we get into galloping mode, we stay there until both runs win less
    often than MIN_GALLOP consecutive times. *)
let min_gallop = 7

(** Maximum initial size of tmp array, which is used for merging. The array can
    grow to accommodate demand. Unlike Tim's original C version, we do not
    allocate this much storage when sorting smaller arrays. This change was
    required for performance. *)
let initial_tmp_storage_length = 256

type 'a instance = {
  (** The array being sorted. *)
  a : 'a array ;

  (** The comparison function. *)
  cmp : 'a cmp ;

  (** This controls when we get *into* galloping mode. It is initialized to
      MIN_GALLOP. The mergeLo and mergeHi methods nudge it higher for random
      data, and lower for highly structured data. *)
  mutable minGallop : int ; (* default: MIN_GALLOP *)

  (** Temp storage for merges. A workspace array may optionally be provided in
      constructor, and if so will be used as long as it is big enough. *)
  mutable tmp : 'a array ;
  mutable tmpBase : int ; (* base of tmp array slice *)
  mutable tmpLen : int ; (* length of tmp array slice *)

  (** A stack of pending runs yet to be merged. Run i starts at address base[i]
      and extends for len[i] elements. It's always true (so long as the indices
      are in bounds) that:

         runBase[i] + runLen[i] == runBase[i + 1]

      so we could cut the storage for this, but it's a minor amount, and keeping
      all the info explicit simplifies the code. *)
  mutable stackSize : int ; (* default: 0 *) (* Number of pending runs on stack *)
  runBase : int array ;
  runLen : int array
}


(** Creates a TimSort instance to maintain the state of an ongoing sort.

    This function is named [Timsort] in the Java implementation.

    @param a the array to be sorted
    @param work a workspace array (slice)
    @param workBase origin of usable space in work array
    @param workLen usable size of work array *)
let make_instance (cmp: 'a cmp) (a: 'a array) (work: 'a array option) (workBase: int) (workLen: int) =

  (* Allocated temp storage (which may be increased later if necessary)*)
  let len = Array.length a in
  let tlen =
    if len < 2 * initial_tmp_storage_length then
      len lsr 1
    else
      initial_tmp_storage_length
  in
  let (tmp, tmpBase, tmpLen) =
    if work = None || workLen < tlen || workBase + tlen > Array.length (Option.get work) then
      (Array.make tlen a.(0), 0, tlen)
    else
      (Option.get work, workBase, workLen)
  in

  let stackLen =
    if len < 120 then 5
    else if len < 1542 then 10
    else if len < 119151 then 24
    else 49
  in
  let runBase = Array.make stackLen 0 in
  let runLen = Array.make stackLen 0 in

  { a; cmp;
    minGallop = min_gallop;
    tmp; tmpBase; tmpLen;
    stackSize = 0; runBase; runLen }

(** Sorts the specified portion of the specified array using a binary insertion
   sort. This is the best method for sorting small numbers of elements. It
   requires O(n log n) compares, but O(n^2) data movement (worst case).

   If the initial part of the specified range is already sorted, this method can
   take advantage of it: the method assumes that the elements from index [lo],
   inclusive, to [start], exclusive are already sorted. *)
let binarySort (cmp: 'a cmp) (a: 'a array) (lo: int) (hi: int) (start: int) =
  assert (lo <= start && start <= hi);

  let start =
    if start = lo then
      start + 1
    else
      start
  in

  for start = start to hi - 1 do
    let pivot = a.(start) in

    (* Set left (and right) to the index where a[start] (pivot) belongs *)
    let left = ref lo in
    let right = ref start in
    assert (!left <= !right);

    (* Invariants:
         pivot >= all in [lo, left).
         pivot <  all in [right, start). *)
    while !left < !right do
      let mid = (!left + !right) lsr 1 in
      if cmp pivot a.(mid) < 0 then
        right := mid
      else
        left := mid + 1
    done;
    assert (!left = !right);

    (* The invariants still hold: pivot >= all in [lo, left) and pivot < all in
       [left, start), so pivot belongs at left. Note that if there are elements
       equal to pivot, left points to the first slot after them -- that's why
       this sort is stable. Slide elements over to make room for pivot. *)
    let n = start - !left in (* The number of elements to move *)
    (* Match is just an optimization for arraycopy in default case *)
    (
      match n with
      | 2 ->
        a.(!left + 2) <- a.(!left + 1);
        a.(!left + 1) <- a.(!left)
      | 1 ->
        a.(!left + 1) <- a.(!left)
      | _ ->
        Array.blit a !left a (!left + 1) n
    );
    a.(!left) <- pivot
  done

(** Reverse the specified range of the specified array.

    @param a the array in which a range is to be reversed
    @param lo the index of the first element in the range to be reversed
    @param hi the index after the last element in the range to be reversed *)
let reverseRange (a: 'a array) (lo: int) (hi: int) =
  let lo = ref lo in
  let hi = ref (hi - 1) in
  while !lo < !hi do
    let t = a.(!lo) in
    a.(!lo) <- a.(!hi);
    a.(!hi) <- t;
    incr lo;
    decr hi
  done

(**
      Returns the length of the run beginning at the specified position in
      the specified array and reverses the run if it is descending (ensuring
      that the run will always be ascending when the method returns).

      A run is the longest ascending sequence with:

         a[lo] <= a[lo + 1] <= a[lo + 2] <= ...

      or the longest descending sequence with:

         a[lo] >  a[lo + 1] >  a[lo + 2] >  ...

      For its intended use in a stable mergesort, the strictness of the
      definition of "descending" is needed so that the call can safely
      reverse a descending sequence without violating stability.

      @param a the array in which a run is to be counted and possibly reversed
      @param lo index of the first element in the run
      @param hi index after the last element that may be contained in the run.
             It is required that {@code lo < hi}.
      @return  the length of the run beginning at the specified position in
               the specified array
*)
let countRunAndMakeAscending (cmp: 'a cmp) (a: 'a array) (lo: int) (hi: int) =
  assert (lo < hi);
  let runHi = ref (lo + 1) in
  if !runHi = hi then
    1
  else
    (
      (* Find end of run, and reverse range if descending *)
      if cmp a.(!runHi) a.(lo) < 0 then (* Descending *)
        (
          incr runHi;
          while !runHi < hi && cmp a.(!runHi) a.(!runHi - 1) < 0 do
            incr runHi
          done;
          reverseRange a lo !runHi
        )
      else (* Ascending *)
        (
          while !runHi < hi && cmp a.(!runHi) a.(!runHi - 1) >= 0 do
            incr runHi
          done
        );

      !runHi - lo;
    )

let minRunLength n =
  assert (n >= 0);
  let n = ref n in
  let r = ref 0 in (* Becomes 1 if any 1 bits are shifted off *)
  while !n >= min_merge do
    r := !n lor 1;
    n := !n lsr 1
  done;
  !n + !r

let pushRun (this: 'a instance) (runBase: int) (runLen: int) =
  this.runBase.(this.stackSize) <- runBase;
  this.runLen.(this.stackSize) <- runLen;
  this.stackSize <- this.stackSize + 1

let gallopLeft (cmp: 'a cmp) (key: 'a) (a: 'a array) (base: int) (len: int) (hint: int) =
  assert (len > 0 && hint >= 0 && hint < len);
  let lastOfs = ref 0 in
  let ofs = ref 1 in
  if cmp key a.(base + hint) > 0 then
    (
      (* Gallop right until a[base+hint+lastOfs] < key <= a[base+hint+ofs] *)
      let maxOfs = len - hint in
      while !ofs < maxOfs && cmp key a.(base + hint + !ofs) > 0 do
        lastOfs := !ofs;
        ofs := (!ofs lsl 1) + 1;
        if !ofs <= 0 then (* int overflow *)
          ofs := maxOfs
      done;
      if !ofs > maxOfs then
        ofs := maxOfs;

      (* Make offsets relative to base *)
      lastOfs := !lastOfs + hint;
      ofs := !ofs + hint
    )
  else (* key <= a[base + hint] *)
    (
      (* Gallop left until a[base+hint-ofs] < key <= a[base+hint-lastOfs] *)
      let maxOfs = hint + 1 in
      while !ofs < maxOfs && cmp key a.(base + hint - !ofs) <= 0 do
        lastOfs := !ofs;
        ofs := (!ofs lsl 1) + 1;
        if !ofs <= 0 then (* int overflow *)
          ofs := maxOfs;
      done;
      if !ofs > maxOfs then
        ofs := maxOfs;

      (* Make offsets relative to base *)
      let tmp = !lastOfs in
      lastOfs := hint - !ofs;
      ofs := hint - tmp
    );
  assert (-1 <= !lastOfs && !lastOfs < !ofs && !ofs <= len);

  (* Now a[base+lastOfs] < key <= a[base+ofs], so key belongs somewhere to the
     right of lastOfs but no farther right than ofs. Do a binary search, with
     invariant a[base + lastOfs - 1] < key <= a[base + ofs]. *)
  incr lastOfs;
  while !lastOfs < !ofs do
    let m = !lastOfs + ((!ofs - !lastOfs) lsr 1) in
    if cmp key a.(base + m) > 0 then
      lastOfs := m + 1 (* a[base + m] < key *)
    else
      ofs := m (* key <= a[base + m] *)
  done;
  assert (!lastOfs = !ofs); (* so a[base + ofs - 1] < key <= a[base + ofs] *)
  !ofs

(**
   Like gallopLeft, except that if the range contains an element equal to
   key, gallopRight returns the index after the rightmost equal element. *)
let gallopRight (cmp: 'a cmp) (key: 'a) (a: 'a array) (base: int) (len: int) (hint: int) =
	assert (len > 0 && hint >= 0 && hint < len);

  let ofs = ref 1 in
  let lastOfs = ref 0 in
  if cmp key a.(base + hint) < 0 then
    (
      (* Gallop left until a[b+hint - ofs] <= key < a[b+hint - lastOfs] *)
      let maxOfs = hint + 1 in
      while !ofs < maxOfs && cmp key a.(base + hint - !ofs) < 0 do
        lastOfs := !ofs;
        ofs := (!ofs lsl 1) + 1;
        if !ofs <= 0 then (* int overflow *)
          ofs := maxOfs
      done;
      if !ofs > maxOfs then
        ofs := maxOfs;

      (* Make offsets relative to b *)
      let tmp = !lastOfs in
      lastOfs := hint - !ofs;
      ofs := hint - tmp;
    )
  else (* a[b + hint] <= key *)
    (
      (* Gallop right until a[b+hint + lastOfs] <= key < a[b+hint + ofs] *)
      let maxOfs = len - hint in
      while !ofs < maxOfs && cmp key a.(base + hint + !ofs) >= 0 do
        lastOfs := !ofs;
        ofs := (!ofs lsl 1) + 1;
        if !ofs <= 0 then (* int overflow *)
          ofs := maxOfs
      done;
      if !ofs > maxOfs then
        ofs := maxOfs;

      (* Make offsets relative to b *)
      lastOfs := !lastOfs + hint;
      ofs := !ofs + hint
    );
  assert (-1 <= !lastOfs && !lastOfs < !ofs && !ofs <= len);

  (* Now a[b + lastOfs] <= key < a[b + ofs], so key belongs somewhere to the
     right of lastOfs but no farther right than ofs. Do a binary search, with
     invariant a[b + lastOfs - 1] <= key < a[b + ofs]. *)
  incr lastOfs;
  while !lastOfs < !ofs do
    let m = !lastOfs + ((!ofs - !lastOfs) lsr 1) in

    if cmp key a.(base + m) < 0 then
      ofs := m (* key < a[b + m] *)
    else
      lastOfs := m + 1 (* a[b + m] <= key *)
  done;
  assert (!lastOfs = !ofs); (* so a[b + ofs - 1] <= key < a[b + ofs] *)
  !ofs

(** Ensures that the external array tmp has at least the specified number of
    elements, increasing its size if necessary. The size increases exponentially
    to ensure amortized linear time complexity. *)
let ensureCapacity (this: 'a instance) (minCapacity: int) =
  if this.tmpLen < minCapacity then
    (
      (* Compute smallest power of 2 > minCapacity *)
      let newSize = ref minCapacity in
      newSize := !newSize lor (!newSize lsr 1);
      newSize := !newSize lor (!newSize lsr 2);
      newSize := !newSize lor (!newSize lsr 4);
      newSize := !newSize lor (!newSize lsr 8);
      newSize := !newSize lor (!newSize lsr 16);
      incr newSize;

      if !newSize < 0 then (* Not bloody likely! *)
        newSize := minCapacity
      else
        newSize := min !newSize (Array.length this.a lsr 1);

      let newArray = Array.make !newSize this.a.(0) in
      this.tmp <- newArray;
      this.tmpLen <- !newSize;
      this.tmpBase <- 0
    );
  this.tmp

exception BreakOuter

(**
   Merges two adjacent runs in place, in a stable fashion.  The first
   element of the first run must be greater than the first element of the
   second run (a[base1] > a[base2]), and the last element of the first run
   (a[base1 + len1-1]) must be greater than all elements of the second run.

   For performance, this method should be called only when len1 <= len2;
   its twin, mergeHi should be called if len1 >= len2.  (Either method
   may be called if len1 == len2.) *)
let mergeLo (this: 'a instance) (base1: int) (len1: int) (base2: int) (len2: int) =
  assert (len1 > 0 && len2 > 0 && base1 + len1 = base2);
  let len1 = ref len1 in
  let len2 = ref len2 in

  (* Copy first run into temp array *)
  let a = this.a in (* For performance *)
  let tmp = ensureCapacity this !len1 in
  let cursor1 = ref this.tmpBase in (* Indexes into tmp array *)
  let cursor2 = ref base2 in   (* Indexes int a *)
  let dest = ref base1 in      (* Indexes int a *)
  Array.blit a base1 tmp !cursor1 !len1;

  (* Move first element of second run and deal with degenerate cases *)
  a.(!dest) <- a.(!cursor2);
  incr dest; incr cursor2;
  decr len2;
  if !len2 = 0 then
    (
      Array.blit tmp !cursor1 a !dest !len1
    )
  else if !len1 = 1 then
    (
      Array.blit a !cursor2 a !dest !len2;
      a.(!dest + !len2) <- tmp.(!cursor1); (* Last elt of run 1 to end of merge *)
    )
  else
    (
      let minGallop = ref this.minGallop in (* Use local variable for performance *)
      (
        try
          while true do
            let count1 = ref 0 in (* Number of times in a row that first run won *)
            let count2 = ref 0 in (* Number of times in a row that second run won *)

            (* Do the straightforward thing until (if ever) one run starts winning
               consistently. *)
            let rec do_while_1 () =
              assert (!len1 > 1 && !len2 > 0);
              if this.cmp a.(!cursor2) tmp.(!cursor1) < 0 then
                (
                  a.(!dest) <- a.(!cursor2);
                  incr dest; incr cursor2;
                  incr count2;
                  count1 := 0;
                  decr len2;
                  if !len2 = 0 then
                    raise BreakOuter
                )
              else
                (
                  a.(!dest) <- tmp.(!cursor1);
                  incr dest; incr cursor1;
                  incr count1;
                  count2 := 0;
                  decr len1;
                  if !len1 = 1 then
                    raise BreakOuter
                );
              if (!count1 lor !count2) < !minGallop then
                do_while_1 ()
            in
            do_while_1 ();

            (*
              One run is winning so consistently that galloping may be a
              huge win. So try that, and continue galloping until (if ever)
              neither run appears to be winning consistently anymore.
             *)
            let rec do_while_2 () =
              assert (!len1 > 1 && !len2 > 0);
              count1 := gallopRight this.cmp a.(!cursor2) tmp !cursor1 !len1 0;
              if (!count1 <> 0) then
                (
                  Array.blit tmp !cursor1 a !dest !count1;
                  dest := !dest + !count1;
                  cursor1 := !cursor1 + !count1;
                  len1 := !len1 - !count1;
                  if !len1 <= 1 then (* len1 = 1 || len1 = 0 *)
                    raise BreakOuter;
                );
              a.(!dest) <- a.(!cursor2);
              incr dest; incr cursor2;
              decr len2;
              if !len2 = 0 then
                raise BreakOuter;

              count2 := gallopLeft this.cmp tmp.(!cursor1) a !cursor2 !len2 0;
              if !count2 <> 0 then
                (
                  Array.blit a !cursor2 a !dest !count2;
                  dest := !dest + !count2;
                  cursor2 := !cursor2 + !count2;
                  len2 := !len2 - !count2;
                  if !len2 = 0 then
                    raise BreakOuter
                );
              a.(!dest) <- tmp.(!cursor1);
              incr dest; incr cursor1;
              decr len1;
              if !len1 = 1 then
                raise BreakOuter;
              decr minGallop;
              if !count1 >= min_gallop || !count2 >= min_gallop then
                do_while_2 ()
            in
            do_while_2 ();
            if !minGallop < 0 then
              minGallop := 0;
            minGallop := !minGallop + 2 (* Penalize for leaving gallop mode *)
          done  (* End of "outer" loop *)
        with
          BreakOuter -> ()
      );
      this.minGallop <- if !minGallop < 1 then 1 else !minGallop; (* Write back to field *)

      if !len1 = 1 then
        (
          assert (!len2 > 0);
          Array.blit a !cursor2 a !dest !len2;
          a.(!dest + !len2) <- tmp.(!cursor1); (* Last elt of run 1 to end of merge *)
        )
      else if !len1 = 0 then
        invalid_arg "Comparison method violates its general contract!"
      else
        (
          assert (!len2 = 0);
          assert (!len1 > 1);
          Array.blit tmp !cursor1 a !dest !len1
        )
    )

(** Like mergeLo, except that this method should be called only if len1 >= len2;
    mergeLo should be called if len1 <= len2. (Either method may be called if
    len1 == len2.) *)
let mergeHi (this: 'a instance) (base1: int) (len1: int) (base2: int) (len2: int) =
  assert (len1 > 0 && len2 > 0 && base1 + len1 = base2);
  let len1 = ref len1 in
  let len2 = ref len2 in

  (* Copy second run into temp array *)
  let a = this.a in (* For performance *)
  let tmp = ensureCapacity this !len2 in
  let tmpBase = this.tmpBase in
  Array.blit a base2 tmp tmpBase !len2;

  let cursor1 = ref (base1 + !len1 - 1) in (* Indexes into a *)
  let cursor2 = ref (tmpBase + !len2 - 1) in (* Indexes into tmp array *)
  let dest = ref (base2 + !len2 - 1) in (* Indexes into a *)

  (* Move last element of first run and deal with degenerate cases *)
  a.(!dest) <- a.(!cursor1);
  decr dest; decr cursor1;
  decr len1;
  if !len1 = 0 then
    (
      Array.blit tmp tmpBase a (!dest - (!len2 - 1)) !len2;
    )
  else if !len2 = 1 then
    (
      dest := !dest - !len1;
      cursor1 := !cursor1 - !len1;
      Array.blit a (!cursor1 + 1) a (!dest + 1) !len1;
      a.(!dest) <- tmp.(!cursor2);
    )
  else
    (
      (* Use local variable for performance *)
      let minGallop = ref this.minGallop in (* ''    ''       ''     ''      '' *)
      (
        try
          while true do
            let count1 = ref 0 in (* Number of times in a row that first run won *)
            let count2 = ref 0 in (* Number of times in a row that second run won *)

            (*
              Do the straightforward thing until (if ever) one run
              appears to win consistently.
             *)
            let rec do_while_1 () =
              assert (!len1 > 0 && !len2 > 1);
              if this.cmp tmp.(!cursor2) a.(!cursor1) < 0 then
                (
                  a.(!dest) <- a.(!cursor1);
                  decr dest; decr cursor1;
                  incr count1;
                  count2 := 0;
                  decr len1;
                  if !len1 = 0 then
                    raise BreakOuter
                )
              else
                (
                  a.(!dest) <- tmp.(!cursor2);
                  decr dest; decr cursor2;
                  incr count2;
                  count1 := 0;
                  decr len2;
                  if !len2 = 1 then
                    raise BreakOuter
                );
              if (!count1 lor !count2) < !minGallop then
                do_while_1 ()
            in
            do_while_1 ();

            (*
              One run is winning so consistently that galloping may be a
              huge win. So try that, and continue galloping until (if ever)
              neither run appears to be winning consistently anymore.
             *)
            let rec do_while_2 () =
              assert (!len1 > 0 && !len2 > 1);
              count1 := !len1 - gallopRight this.cmp tmp.(!cursor2) a base1 !len1 (!len1 - 1);
              if !count1 <> 0 then
                (
                  dest := !dest - !count1;
                  cursor1 := !cursor1 - !count1;
                  len1 := !len1 - !count1;
                  Array.blit a (!cursor1 + 1) a (!dest + 1) !count1;
                  if !len1 = 0 then
                    raise BreakOuter
                );
              a.(!dest) <- tmp.(!cursor2);
              decr dest; decr cursor2;
              decr len2;
              if !len2 = 1 then
                raise BreakOuter;

              count2 := !len2 - gallopLeft this.cmp a.(!cursor1) tmp tmpBase !len2 (!len2 - 1);
              if !count2 <> 0 then
                (
                  dest := !dest - !count2;
                  cursor2 := !cursor2 - !count2;
                  len2 := !len2 - !count2;
                  Array.blit tmp (!cursor2 + 1) a (!dest + 1) !count2;
                  if !len2 <= 1 then (* len2 = 1 || len2 = 0 *)
                    raise BreakOuter
                );
              a.(!dest) <- a.(!cursor1);
              decr dest; decr cursor1;
              decr len1;
              if !len1 = 0 then
                raise BreakOuter;
              decr minGallop;
              if (!count1 >= min_gallop || !count2 >= min_gallop) then
                do_while_2 ()
            in
            do_while_2 ();
            if !minGallop < 0 then
              minGallop := 0;
            minGallop := !minGallop + 2; (* Penalize for leaving gallop mode *)
          done (* End of "outer" loop *)
        with
          BreakOuter -> ()
      );
      this.minGallop <- if !minGallop < 1 then 1 else !minGallop; (* Write back to field *)

      if !len2 = 1 then
        (
          assert (!len1 > 0);
          dest := !dest - !len1;
          cursor1 := !cursor1 - !len1;
          Array.blit a (!cursor1 + 1) a (!dest + 1) !len1;
          a.(!dest) <- tmp.(!cursor2); (* Move first elt of run2 to front of merge *)
        )
      else if !len2 = 0 then
        invalid_arg "Comparison method violates its general contract!"
      else
        (
          assert (!len1 = 0);
          assert (!len2 > 0);
          Array.blit tmp tmpBase a (!dest - (!len2 - 1)) !len2
        )
    )

(** Merges the two runs at stack indices i and i+1. Run i must be the
    penultimate or antepenultimate run on the stack. In other words, i must be
    equal to stackSize-2 or stackSize-3. *)
let mergeAt (this: 'a instance) (i: int) =
  assert (this.stackSize >= 2);
  assert (i >= 0);
  assert (i = this.stackSize - 2 || i = this.stackSize - 3);

  let base1 = ref this.runBase.(i) in
  let len1 = ref this.runLen.(i) in
  let base2 = this.runBase.(i+1) in
  let len2 = this.runLen.(i+1) in
  assert (!len1 > 0 && len2 > 0);
  assert (!base1 + !len1 = base2);

  (* Record the length of the combined runs; if i is the 3rd-last run now, also
     slide over the last run (which isn't involved in this merge). The current
     run (i+1) goes away in any case. *)
  this.runLen.(i) <- !len1 + len2;
  if i = this.stackSize - 3 then
    (
      this.runBase.(i+1) <- this.runBase.(i+2);
      this.runLen.(i+1) <- this.runLen.(i+2)
    );
  this.stackSize <- this.stackSize - 1;

  (* Find where the first element of run2 goes in run1. Prior elements in run1
     can be ignored (because they're already in place). *)
  let k = gallopRight this.cmp this.a.(base2) this.a !base1 !len1 0 in
  assert (k >= 0);
  base1 := !base1 + k;
  len1 := !len1 - k;
  if !len1 = 0 then
    ()
  else
    (
      (* Find where the last element of run1 goes in run2. Subsequent elements
         in run2 can be ignored (because they're already in place). *)
      let len2 = gallopLeft this.cmp this.a.(!base1 + !len1 - 1) this.a base2 len2 (len2-1) in
      assert (len2 >= 0);
      if len2 = 0 then
        ()
      else
        (
          (* Merge remaining runs, using tmp array with min(len1, len2) elements *)
          if !len1 <= len2 then
            mergeLo this !base1 !len1 base2 len2
          else
            mergeHi this !base1 !len1 base2 len2
        )
    )

let mergeCollapse (this: 'a instance) =
  (* while (stackSize > 1) *)
  let rec while_ () =
    if this.stackSize > 1 then
      (
        let n = ref (this.stackSize - 2) in
        if !n > 0 && this.runLen.(!n-1) <= this.runLen.(!n) + this.runLen.(!n+1) then
          (
            if this.runLen.(!n-1) < this.runLen.(!n+1) then
              decr n;
            mergeAt this !n;
            while_ ()
          )
        else if this.runLen.(!n) <= this.runLen.(!n+1) then
          (
            mergeAt this !n;
            while_ ()
          )
        else
          ()
      )
  in
  while_ ()

(** Merges all runs on the stack until only one remains. This method is called
    once, to complete the sort. *)
let mergeForceCollapse (this: 'a instance) =
  while this.stackSize > 1 do
    let n = ref (this.stackSize - 2) in
    if !n > 0 && this.runLen.(!n-1) < this.runLen.(!n+1) then
      decr n;
    mergeAt this !n
  done

(** Sorts the given range, using the given workspace array slice for temp
    storage when possible. This method is designed to be invoked from public
    methods (in class Arrays) after performing any necessary array bounds checks
    and expanding parameters into the required forms. *)
let sort (cmp: 'a cmp) (a: 'a array) (lo: int) (hi: int) =
  assert (lo >= 0 && lo <= hi && hi <= Array.length a);

  let nRemaining = hi - lo in
  if nRemaining < 2 then
    () (* Arrays of size 0 and 1 are always sorted *)
  else if nRemaining < min_merge then
    (
      (* If array is small, do a "mini-TimSort" with no merges *)
      let initRunLen = countRunAndMakeAscending cmp a lo hi in
      binarySort cmp a lo hi (lo + initRunLen);
    )
  else
    (
      (* March over the array once, left to right, finding natural runs,
         extending short natural runs to minRun elements, and merging runs to
         maintain stack invariant. *)

      let lo = ref lo in
      let nRemaining = ref nRemaining in

      let ts = make_instance cmp a None 0 0 in
      let minRun = minRunLength !nRemaining in

      (* do ... while (nRemaining != 0) *)
      let rec do_while () =
        (* Identify next run *)
        let runLen = countRunAndMakeAscending cmp a !lo hi in

        let runLen =
          (* If run is short, extend to min(minRun, nRemaining) *)
          if runLen < minRun then
            (
              let force = if !nRemaining <= minRun then !nRemaining else minRun in
              binarySort cmp a !lo (!lo + force) (!lo + runLen);
              force
            )
          else
            runLen
        in

        (* Push run onto pending-run stack, and maybe merge *)
        pushRun ts !lo runLen;
        mergeCollapse ts;

        (* Advance to find next run *)
        lo := !lo + runLen;
        nRemaining := !nRemaining - runLen;

        if !nRemaining <> 0 then
          do_while ()
      in
      do_while ();

      (* Merge all remaining runs to complete sort *)
      assert (!lo = hi);
      mergeForceCollapse ts;
      assert (ts.stackSize = 1)
    )

(** This function is not given like this in the Java implementation but is here
    for interoperability with the OCaml way of presenting sorting algorithms. *)
let sort (cmp: 'a cmp) (a: 'a array) =
  (* The index of the last element is exclusive in this sort. *)
  if a <> [||] then
    sort cmp a 0 (Array.length a)
