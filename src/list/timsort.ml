(* Decomposition of the initial list in runs. *)

type 'a run =
  | Asc of 'a list
  | Desc of 'a list

let rec next_asc_run_rev compare revrun last len = function
  | x :: xs when compare last x <= 0 ->
    next_asc_run_rev compare (last :: revrun) x (len + 1) xs
  | xs -> len, Desc (last :: revrun), xs

let rec next_desc_run_rev compare revrun last len = function
  | x :: xs when compare last x > 0 ->
    next_desc_run_rev compare (last :: revrun) x (len + 1) xs
  | xs -> len, Asc (last :: revrun), xs

let next_run compare xs =
  match xs with
  | [] -> None
  | [_] -> Some (1, Asc xs, [])
  | x1 :: x2 :: xs ->
    if compare x1 x2 <= 0
    then Some (next_asc_run_rev compare [x1] x2 2 xs)
    else Some (next_desc_run_rev compare [x1] x2 2 xs)

(* Merge function (as in MergeSort) *)

let merge_asc_rev compare =
  let rec merge_asc_rev revacc xs ys =
    match xs, ys with
    | ([], zs | zs, []) -> List.rev_append zs revacc
    | x :: xs', y :: ys' ->
      if compare x y <= 0
      then merge_asc_rev (x :: revacc) xs' ys
      else merge_asc_rev (y :: revacc) xs ys'
  in
  merge_asc_rev []

let merge_desc_rev compare =
  let rec merge_desc_rev revacc xs ys =
    match xs, ys with
    | ([], zs | zs, []) -> List.rev_append zs revacc
    | x :: xs', y :: ys' ->
      if compare x y > 0
      then merge_desc_rev (x :: revacc) xs' ys
      else merge_desc_rev (y :: revacc) xs ys'
  in
  merge_desc_rev []

let merge compare n1 n2 r1 r2 =
  let run =
    match r1, r2 with
    | Asc r1, Asc r2 -> Desc (merge_asc_rev compare r1 r2)
    | Desc r1, Desc r2 -> Asc (merge_desc_rev compare r1 r2)
    | Asc r1, Desc r2 ->
      if n1 < n2
      then Asc (merge_desc_rev compare (List.rev r1) r2)
      else Desc (merge_asc_rev compare r1 (List.rev r2))
    | Desc r1, Asc r2 ->
      if n1 < n2
      then Desc (merge_asc_rev compare (List.rev r1) r2)
      else Asc (merge_desc_rev compare r1 (List.rev r2))
  in
  (n1 + n2), run

(* The TimSort algorithm. *)

let timsort compare =
  let merge = merge compare in
  let next_run = next_run compare in

  let rec fetch_next_run remaining stack =
    match next_run remaining with
    | Some (n, r, rem) -> sort rem ((n, r) :: stack)
    | None -> stack

  and sort remaining stack =
    match stack with
    | ((n1, r1) as s1) :: (n2, r2) :: (((n3, r3) :: stack'') as stack') ->
      if n3 < n1 then
        sort remaining (s1 :: merge n2 n3 r2 r3 :: stack'')
      else if n2 <= n1 then
        sort remaining (merge n1 n2 r1 r2 :: stack')
      else if n3 <= n1 + n2 then
        sort remaining (merge n1 n2 r1 r2 :: stack')
      else begin match stack with
        | (n4, _) :: _ when n4 <= n3 + n2 ->
          sort remaining (merge n1 n2 r1 r2 :: stack')
        | _ -> fetch_next_run remaining stack
      end
    | [(n1, r1); (n2, r2)] when n2 <= n1 ->
      sort remaining [merge n1 n2 r1 r2]
    | _ -> fetch_next_run remaining stack
  in

  let rec finish = function
    | (n1, r1) :: (n2, r2) :: stack ->
      finish (merge n1 n2 r1 r2 :: stack)
    | [(_, Asc r)] -> r
    | [(_, Desc r)] -> List.rev r
    | [] -> []
  in

  fun l -> finish (sort l [])
