type 'a cmp = 'a -> 'a -> int

let reverse_inplace (t: 'a array) (offset: int) (len: int) =
  assert (0 <= offset);
  assert (len >= 0);
  assert (offset + len <= Array.length t);
  (* ensures: (t after)[offset:offset+len] = (t before)[offset:offset+len:-1] *)
  for i = 0 to (len - 1) / 2 do
    let tmp = t.(offset + i) in
    t.(offset + i) <- t.(offset + len - 1 - i);
    t.(offset + len - 1 - i) <- tmp
  done

let%test _ =
  let t = [|1; 2; 3; 4; 5; 6|] in
  reverse_inplace t 1 3;
  t = [|1; 4; 3; 2; 5; 6|]

let%test _ =
  let t = [|1; 2; 3; 4; 5; 6|] in
  reverse_inplace t 1 4;
  t = [|1; 5; 4; 3; 2; 6|]

let next_run (cmp: 'a cmp) (t: 'a array) (offset: int) : int =
  assert (0 <= offset);
  assert (offset < Array.length t);
  (* ensures: t[offset: offset + return] is the first run of t[offset:]. *)
  let t_len = Array.length t in

  let rec next_asc_run prec ofs =
    if ofs >= t_len || cmp prec t.(ofs) > 0
    then ofs - offset
    else next_asc_run t.(ofs) (ofs + 1)
  in

  let rec next_desc_run prec ofs =
    if ofs >= t_len || cmp prec t.(ofs) <= 0
    then ofs - offset
    else next_desc_run t.(ofs) (ofs + 1)
  in

  if t_len = offset + 1 then 1
  else
    let prec = t.(offset + 1) in
    if cmp t.(offset) prec <= 0 then
      next_asc_run prec (offset + 2)
    else begin
      let len = next_desc_run prec (offset + 2) in
      reverse_inplace t offset len;
      len
    end

let%test_module _ =
  (module struct
    let t () = [|1; 4; 6; 9; 2; 4; 0; 9|]
    let cmp = Int.compare

    let%test _ = next_run cmp (t ()) 0 = 4
    let%test _ = next_run cmp (t ()) 1 = 3
    let%test _ = next_run cmp (t ()) 3 = 2 (* descending run *)
    let%test _ = next_run cmp (t ()) 4 = 2
  end)

let rec merge_lo
  cmp
  dest beg
  src0 beg0 end0 x0
  src1 beg1 end1 x1
=
  assert (Array.length dest >= beg + (end0 - beg0 + 1) + (end1 - beg1 + 1));
  assert (Array.length src0 > end0);
  assert (Array.length src1 > end1);
  assert (x0 = src0.(beg0));
  assert (x1 = src1.(beg1));
  assert (end0 >= beg0);
  assert (end1 >= beg1);

  (* This is used to optimise the case len0 = 1 below. *)
  assert (dest == src1 && beg + (end0 - beg0 + 1) = beg1);

  if cmp x0 x1 <= 0 then begin
    dest.(beg) <- x0;
    (* If this was the last element of run0 we stop. Besides, since we have
     * [dest == src1], the remaining element of run1 are already where they
     * are supposed to be and we can exit. *)
    if beg0 < end0 then
      merge_lo
        cmp
        dest (beg + 1)
        src0 (beg0 + 1) end0 src0.(beg0 + 1)
        src1 beg1 end1 x1
  end else begin
    dest.(beg) <- x1;
    (* If this was the last element of run1 we stop. There remains to move all
     * the elements of run0 at the beginning of dest. *)
    if beg1 < end1 then
      merge_lo
        cmp
        dest (beg + 1)
        src0 beg0 end0 x0
        src1 (beg1 + 1) end1 src1.(beg1 + 1)
    else
      Array.blit src0 beg0 dest (beg + 1) (end0 - beg0 + 1)
  end

let rec merge_hi
  cmp
  dest end_
  src0 beg0 end0 x0 (* run0 *)
  src1 beg1 end1 x1 (* run1 *)
=
  assert (Array.length dest > end_);
  assert (Array.length src0 > end0);
  assert (Array.length src1 > end1);
  assert (x0 = src0.(end0));
  assert (x1 = src1.(end1));
  assert (end0 >= beg0);
  assert (end1 >= beg1);
  (* This is used to optimise the case len1 = 1 below. *)
  assert (dest == src0 && end_ - (end1 - beg1 + 1) = end0);

  if cmp x0 x1 <= 0 then begin
    dest.(end_) <- x1;
    (* If this was the last element of run1 we stop. Besides, since we have
     * [dest == src0], the remaining element of run0 are already where they
     * are supposed to be and we can exit. *)
    if beg1 < end1 then
      merge_hi
        cmp
        dest (end_ - 1)
        src0 beg0 end0 x0
        src1 beg1 (end1 - 1) src1.(end1 - 1)
  end else begin
    dest.(end_) <- x0;
    (* If this was the last element of run0 we stop. There remains to move all
     * the elements of run1 at the end of dest. *)
    if beg0 < end0 then
      merge_hi
        cmp
        dest (end_ - 1)
        src0 beg0 (end0 - 1) src0.(end0 - 1)
        src1 beg1 end1 x1
    else
      let len1 = end1 - beg1 + 1 in
      Array.blit src1 beg1 dest (end_ - len1) len1
  end


let merge ~buffer cmp t (ofs0, len0) (ofs1, len1) =
  assert (ofs0 + len0 = ofs1);
  assert (ofs1 + len1 <= Array.length t);
  if len0 < len1 then begin
    Array.blit t ofs0 buffer 0 len0;
    merge_lo
      cmp
      t ofs0
      buffer 0 (len0 - 1) buffer.(0)
      t ofs1 (ofs1 + len1 - 1) t.(ofs1)
  end else begin
    Array.blit t ofs1 buffer 0 len1;
    let top0 = ofs0 + len0 - 1 in
    merge_hi
      cmp
      t (top0 + len1)
      t ofs0 top0 t.(top0)
      buffer 0 (len1 - 1) buffer.(len1 - 1)
  end;
  ofs0, len0 + len1

let rec merge_all ~buffer cmp t = function
  | [] ->
    assert (t = [||]);
    ()
  | [(ofs, len)] ->
    assert (ofs = 0);
    assert (len = Array.length t);
    ()
  | r0 :: r1 :: stack ->
    merge_all ~buffer cmp t (merge ~buffer cmp t r1 r0 :: stack)

let timsort (cmp: 'a cmp) (t: 'a array) =
  let t_len = Array.length t in
  (* Merge buffer: when merging two adjacent runs, the smallest one is moved to
   * this temporary buffer and the merging happens in the main array. *)
  let buffer =
    if t_len > 2 then Array.make (t_len / 2) t.(0)
    else [||]
  in
  let merge = merge ~buffer cmp t in
  let merge_all = merge_all ~buffer cmp t in
  let next_run = next_run cmp t in

  let rec sort offset (stack0 : (int * int) list) =
    (* stackn = stack starting with rn *)
    match stack0 with
    | ((_, len0) as r0) :: (((_, len1) as r1) :: stack2) ->
      (
        match stack2 with
        | ((_, len2) as r2) :: stack3 ->
          (
            if len2 < len0 then
              sort offset (r0 :: merge r2 r1 :: stack3)
            else if len1 <= len0 then
              sort offset (merge r1 r0 :: stack2)
            else if len2 <= len1 + len0 then
              sort offset (merge r1 r0 :: stack2)
            else
              match stack3 with
              | (_, len3) :: _ when len3 <= len2 + len1 ->
                sort offset (merge r1 r0 :: stack2)

              | _ ->
                if offset < t_len then
                  let len = next_run offset in
                  sort (offset + len) ((offset, len) :: stack0)
                else
                  merge_all stack0
          )
        | [] ->
          if len1 <= len0 then
            sort offset (merge r1 r0 :: stack2)
          else if offset < t_len then
            let len = next_run offset in
            sort (offset + len) ((offset, len) :: stack0)
          else
            merge_all stack0
      )

    | _ ->
      if offset < t_len then
        let len = next_run offset in
        sort (offset + len) ((offset, len) :: stack0)
      else
        merge_all stack0
  in

  sort 0 []


let%test _ =
  let t = [|0;6;2;6;4;1;9;7|] in
  let t_sorted = Array.copy t in
  Array.sort Int.compare t_sorted;
  timsort Int.compare t;
  t = t_sorted

let%test _ =
  let t = Array.mapi (fun i x -> i, x) [|0;6;2;6;0;1;0;2|] in
  let t_sorted = Array.copy t in
  let cmp (_, x) (_, y) = Int.compare x y in
  Array.stable_sort cmp t_sorted;
  timsort cmp t;
  t = t_sorted

let%test _ =
  timsort compare [||]; true

let%test _ =
  let t = [|true|] in
  timsort Bool.compare t;
  t.(0)
