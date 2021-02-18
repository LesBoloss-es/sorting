module Timsort

open FStar.Seq

type cmp (a:Type) = (f:(a -> a -> int){
//    (forall a. f a a == 0)                                  (* reflexivity   *) (implied by following 2)
  (forall a1 a2. f a1 a2 > 0  <==> f a2 a1 < 0)  (* anti-symmetry *)
  /\ (forall a1 a2. f a1 a2 == 0  <==> f a2 a1 == 0)})  (* symmetrish *)
//    /\ (forall a1 a2 a3. f a1 a2 /\ f a2 a3 ==> f a1 a3)        (* transitivity  *)

type ofs_t (#a:Type) (s:seq a) = i:nat{i < length s}
type len_t (#a:Type) (s:seq a) = n:nat{n == length s}
type slice_t (#a:Type) (s:seq a) = (p:(nat * nat){fst p + snd p <= length s /\ snd p <> 0})

let modifies_slice (#a:Type) (s:seq a) (s':seq a{length s == length s'}) (beg end_:ofs_t s) =
  forall (i:ofs_t s). i < beg \/ i > end_ ==> index s i == index s' i

// Important: the upper bound of slice is not included. Hence the +1 on end_
let is_permutation_aux (#a:eqtype)
  (s:seq a) (s':seq a{length s == length s'})
  (beg:ofs_t s) (end_:ofs_t s)
  (x:a)
  : prop
  = beg <= end_ ==> count x (slice s beg (end_+1)) == count x (slice s' beg (end_+1))

let is_permutation (#a:eqtype) s s' beg end_ = forall (x:a). is_permutation_aux s s' beg end_ x

#push-options "--z3rlimit 20 --fuel 1 --ifuel 0"

let rec reverse_inplace_aux (#a:eqtype) (s:seq a) (beg end_:ofs_t s)
  : Tot (s':seq a{length s' == length s /\ modifies_slice s s' beg end_})
        (decreases end_)
  = if beg >= end_ then s
    else
      let tmp = index s beg in
      let s = upd s beg (index s end_) in
      let s = upd s end_ tmp in
      reverse_inplace_aux s (beg + 1) (end_ - 1)

let reverse_inplace (#a:eqtype) (s:seq a) (slice:slice_t s) =
  let ofs, len = slice in
  reverse_inplace_aux s ofs (ofs + len - 1)

let is_increasing_aux (#a:eqtype) (cmp:cmp a) (s:seq a) (beg end_:ofs_t s) (i j:nat) : prop =
  (beg <= i /\ i < j /\ j <= end_) ==> cmp (index s i) (index s j) <= 0

let is_increasing cmp s beg end_ = forall i j. is_increasing_aux cmp s beg end_ i j

let is_decreasing_aux (#a:eqtype) (cmp:cmp a) (s:seq a) (beg end_:ofs_t s) (i j:nat) : prop =
  (beg <= i /\ i < j /\ j <= end_) ==> cmp (index s i) (index s j) > 0

let is_decreasing cmp s beg end_ = forall (i j:nat). is_decreasing_aux cmp s beg end_ i j

let extract_original_index (#a:eqtype) (s s':seq a) (beg end_:ofs_t s) (j:ofs_t s)
  : Ghost nat
          (requires length s == length s' /\
            is_permutation s s' beg end_ /\
            j >= beg /\
            j <= end_)
          (ensures fun j' -> j' >= beg /\ j' <= end_ /\ index s' j == index s j')
 = admit()

let smaller_decreasing (#a:eqtype) (cmp:cmp a) (s:seq a) (beg end_:ofs_t s)
  : Lemma (requires beg < end_ /\ is_decreasing cmp s beg end_)
          (ensures is_decreasing cmp s (beg+1) (end_ - 1))
  = admit()

let frame_decreasing (#a:eqtype) (cmp:cmp a) (s s':seq a) (beg end_:ofs_t s)
  : Lemma (requires is_decreasing cmp s beg end_ /\
            length s == length s' /\
            (forall j. j >= beg /\ j <= end_ ==> index s j == index s' j))
          (ensures is_decreasing cmp s' beg end_)
  = admit()

let frame_count (#a:eqtype) (s s':seq a) (beg end_:ofs_t s) (x:a)
  : Lemma (requires length s == length s' /\
            (forall j. j >= beg /\ j <= end_ ==> index s j == index s' j))
          (ensures beg <= end_ ==> count x (slice s' beg (end_+1)) == count x (slice s beg (end_+1)))
  = admit()

let lemma_count_append2 (#a:eqtype) (s:seq a) (beg end_:ofs_t s) (x:a)
  : Lemma (requires beg+1 <= end_-1)
    (ensures count x (slice s beg (end_+1)) ==
      (if index s beg = x then 1 else 0) +
      (if index s end_ = x then 1 else 0) +
      count x (slice s (beg+1) (end_)))
  = admit()

let lemma_count_2elem (#a:eqtype) (s:seq a) (beg:ofs_t s) (x:a)
  : Lemma (requires beg + 1 < length s)
    (ensures count x (slice s beg (beg+2)) ==
      (if index s beg = x then 1 else 0) +
      (if index s (beg+1) = x then 1 else 0))
  = let s' = slice s beg (beg+2) in
    let s_lo = slice s beg (beg+1) in
    let s_hi = slice s (beg+1) (beg+2) in
    assert (equal s' (append s_lo s_hi));
    lemma_append_count s_lo s_hi

val lemma_reverse_permutation_aux (#a:eqtype) (s:seq a) (beg end_:ofs_t s) (x:a) :
  Lemma
    (ensures is_permutation_aux s (reverse_inplace_aux s beg end_) beg end_ x)
    (decreases end_)

let rec lemma_reverse_permutation_aux s beg end_ x =
  if beg >= end_ then ()
  else (
    let tmp = index s beg in
    let s1 = upd s beg (index s end_) in
    let s2 = upd s1 end_ tmp in
    let sf = reverse_inplace_aux s2 (beg + 1) (end_ - 1) in
    frame_count s s2 (beg+1) (end_-1) x;
    lemma_reverse_permutation_aux s2 (beg+1) (end_-1) x;


    if beg+1 <= end_-1 then (
      lemma_count_append2 s beg end_ x;
      lemma_count_append2 s2 beg end_ x;
      lemma_count_append2 sf beg end_ x
    ) else (
      assert (equal s2 sf);
      lemma_count_2elem s beg x;
      lemma_count_2elem s2 beg x
    )
  )

val lemma_reverse_permutation (#a:eqtype) (s:seq a) (beg end_:ofs_t s) :
  Lemma (is_permutation s (reverse_inplace_aux s beg end_) beg end_)

let lemma_reverse_permutation s beg end_ =
  Classical.forall_intro (lemma_reverse_permutation_aux s beg end_)

val lemma_reverse_increasing_aux (#a:eqtype) (cmp:cmp a) (s:seq a) (beg end_:ofs_t s) (i j:nat)
  : Lemma (requires is_decreasing cmp s beg end_)
          (ensures is_increasing_aux cmp (reverse_inplace_aux s beg end_) beg end_ i j)
          (decreases end_)

let rec lemma_reverse_increasing_aux cmp s beg end_ i j =
  assert (is_decreasing_aux cmp s beg end_ i j);
  if beg >= end_ || i < beg || j > end_ || i >= j  then ()
  else (
    if beg < i && j < end_ then (
      smaller_decreasing cmp s beg end_;
      let tmp = index s beg in
      let s1 = upd s beg (index s end_) in
      let s2 = upd s1 end_ tmp in
      frame_decreasing cmp s s2 (beg + 1) (end_ - 1);
      lemma_reverse_increasing_aux cmp s2 (beg + 1) (end_ -1) i j
    ) else if i = beg && j = end_ then ( ()
    ) else (
      let tmp = index s beg in
      let s1 = upd s beg (index s end_) in
      let s2 = upd s1 end_ tmp in
      let sf = reverse_inplace_aux s2 (beg + 1) (end_ - 1) in
      lemma_reverse_permutation s2 (beg+1) (end_ - 1);
      if i = beg then (
        let j' = extract_original_index s2 sf (beg+1) (end_-1) j in
        assert (is_decreasing_aux cmp s beg end_ j' end_)
      ) else (
        let i' = extract_original_index s2 sf (beg+1) (end_-1) i in
        assert (is_decreasing_aux cmp s beg end_ beg i')
      )
    )
  )


val lemma_reverse_increasing (#a:eqtype) (cmp:cmp a) (s:seq a) (beg end_:ofs_t s)
  : Lemma (requires is_decreasing cmp s beg end_)
          (ensures is_increasing cmp (reverse_inplace_aux s beg end_) beg end_)
          (decreases end_)

let lemma_reverse_increasing cmp s beg end_ = Classical.forall_intro_2 (Classical.move_requires_2 (lemma_reverse_increasing_aux cmp s beg end_))
