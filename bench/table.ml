let pf = Format.printf
let fpf = Format.fprintf
let spf = Format.sprintf

let pp_int = Format.pp_print_int
let pp_string = Format.pp_print_string

type cell = Cell : (unit -> 'a) * ('a -> string) -> cell
type row = cell list
type table = row list

let cell ~to_string f = Cell (f, to_string)

let fmt_cell ~fmt f = cell ~to_string:(spf fmt) f

let int_cell i = cell ~to_string:string_of_int (fun () -> i)
let string_cell s = cell ~to_string:Fun.id (fun () -> s)

type cell_size = None | Given of int | Auto of int ref
type align = Left | Center | Right

let fill_string ?(fill=' ') size = String.make size fill

let align_string ?fill align str size =
  let len = String.length str in
  match align with
  | Left -> str ^ (fill_string ?fill (size - len))
  | Right -> (fill_string ?fill (size - len)) ^ str
  | Center -> (fill_string ?fill ((size - len) / 2)) ^ str ^ (fill_string ?fill (size - len - (size - len) / 2))

let pp_cell ~size ~align fmt = function
  | Cell (f, to_string) ->
    let str = to_string (f ()) in
    let len = String.length str in
    let size =
      match size with
      | None -> len
      | Auto size -> size := max !size len; !size
      | Given size -> size
    in
    let str = align_string align str size in
    pp_string fmt str

let pp_row ~sep_len ?cell_styles fmt cells =
  match cells with
  | [] -> ()
  | cell :: cells ->
    let ((size, align), styles) =
      match cell_styles with
      | None -> ((None, Left), List.map (fun _ -> (None, Left)) cells)
      | Some ((size, align) :: styles) -> ((size, align), styles)
      | _ -> invalid_arg "pp_row"
    in
    pp_cell ~size ~align fmt cell;

    List.iter2
      (fun cell (size, align) ->
         fpf fmt "%s@?" (String.make sep_len ' ');
         pp_cell ~size ~align fmt cell)
      cells styles;
    fpf fmt "@."

let pp_table ?title ?headers ?(sep_len=3) ?cell_styles fmt t =
  let len () =
    match cell_styles with
    | None -> 0
    | Some cell_styles ->
      List.fold_left
        (fun len (size, _align) ->
           len + sep_len
           + match size with
             | None -> 0
             | Auto size -> !size
             | Given size -> size)
        (-sep_len) cell_styles
  in
  (match title with
   | None -> ()
   | Some title -> fpf fmt "%s@." (align_string Center (spf " [ %s ] " title) (len ()) ~fill:'='));
  (match headers with
   | None -> ()
   | Some headers -> pp_row ~sep_len ?cell_styles fmt headers);
  fpf fmt "%s@." (String.make (len ()) '-');
  List.iter (pp_row ~sep_len ?cell_styles fmt) t;
  fpf fmt "%s@." (String.make (len ()) '-')

let print_table ?title ?headers ?cell_styles t =
  pp_table ?title ?headers ?cell_styles Format.std_formatter t
