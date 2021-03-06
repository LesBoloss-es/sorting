type 'a cmp = 'a -> 'a -> int

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
      ReverseInplace.reverse_inplace t offset len;
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
  dest ofs
  src0 ofs0 len0
  src1 ofs1 len1
=
  assert (Array.length dest >= ofs + len0 + len1);
  assert (Array.length src0 >= ofs0 + len0);
  assert (Array.length src1 >= ofs1 + len1);
  if len0 = 0 then
    Array.blit src1 ofs1 dest ofs len1
  else if len1 = 0 then
    Array.blit src0 ofs0 dest ofs len0
  else if cmp src0.(ofs0) src1.(ofs1) <= 0 then begin
    dest.(ofs) <- src0.(ofs0);
    merge_lo
      cmp
      dest (ofs + 1)
      src0 (ofs0 + 1) (len0 - 1)
      src1 ofs1 len1
  end else begin
    dest.(ofs) <- src1.(ofs1);
    merge_lo
      cmp
      dest (ofs + 1)
      src0 ofs0 len0
      src1 (ofs1 + 1) (len1 - 1)
  end


let rec merge_hi
  cmp
  dest ofs
  src0 ofs0 len0
  src1 ofs1 len1
=
  assert (Array.length dest >= ofs + len0 + len1);
  assert (Array.length src0 >= ofs0 + len0);
  assert (Array.length src1 >= ofs1 + len1);
  assert (len0 >= 0);
  assert (len1 >= 0);
  if len0 = 0 then
    Array.blit src1 ofs1 dest ofs len1
  else if len1 = 0 then
    Array.blit src0 ofs0 dest ofs len0
  else if cmp src0.(ofs0 + len0 - 1) src1.(ofs1 + len1 - 1) <= 0 then begin
    dest.(ofs + len0 + len1 - 1) <- src1.(ofs1 + len1 - 1);
    merge_hi
      cmp
      dest ofs
      src0 ofs0 len0
      src1 ofs1 (len1 - 1)
  end else begin
    dest.(ofs + len0 + len1 - 1) <- src0.(ofs0 + len0 - 1);
    merge_hi
      cmp
      dest ofs
      src0 ofs0 (len0 - 1)
      src1 ofs1 len1
  end


let merge ~buffer cmp t (ofs0, len0) (ofs1, len1) =
  assert (ofs0 + len0 = ofs1);
  assert (ofs1 + len1 <= Array.length t);
  if len0 < len1 then begin
    Array.blit t ofs0 buffer 0 len0;
    merge_lo
      cmp
      t ofs0
      buffer 0 len0
      t ofs1 len1
  end else begin
    Array.blit t ofs1 buffer 0 len1;
    merge_hi
      cmp
      t ofs0
      t ofs0 len0
      buffer 0 len1
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
