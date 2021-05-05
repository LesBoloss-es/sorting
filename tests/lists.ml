open Sorting

let rand_large_int_list len =
  List.init len (fun _ -> Random.bits ())

let rand_small_int_list len =
  List.init len (fun _ -> Random.int (max 10 (len / 10)))

let test_compare_one l =
  let pp_list =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
      Format.pp_print_int
  in

  let res = Timsort.timsort Int.compare l in
  if List.sort Int.compare l <> res then begin
    Format.eprintf "Error on [%a]@." pp_list l;
    Format.eprintf "Found: [%a]@." pp_list res;
    exit 1
  end

let test_correct nb =
  for i = 1 to nb do
    let len = Random.int (2 * i) + (2 * i) in
    test_compare_one (rand_large_int_list len);
    test_compare_one (rand_small_int_list len)
  done;
  Format.printf "@."

let () =
  Random.self_init ();
  test_correct 500
