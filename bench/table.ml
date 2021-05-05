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

let repeat_string ?(fill=" ") size =
  let output = Buffer.create size in
  for _ = 1 to size do Buffer.add_string output fill; done;
  Buffer.contents output

let align_string ?fill align str size =
  let len = String.length str in
  match align with
  | Left -> str ^ (repeat_string ?fill (size - len))
  | Right -> (repeat_string ?fill (size - len)) ^ str
  | Center -> (repeat_string ?fill ((size - len) / 2)) ^ str ^ (repeat_string ?fill (size - len - (size - len) / 2))

let cell_to_string = function
  | Cell (f, to_string) -> to_string (f ())

let pp_table ?title ~cell_styles ~heading fmt content =
  let cell_styles = Array.of_list cell_styles in
  let cell_widths = Array.map (fun _ -> 0) cell_styles in

  (* Render cells + compute width of columns *)

  let render_row row =
    row |> List.mapi @@ fun i cell ->
    let str = cell_to_string cell in
    cell_widths.(i) <- max cell_widths.(i) (String.length str);
    str
  in
  let heading = render_row heading in
  let content = List.map render_row content in

  (* Compute the row template of the table *)
  let row_tmpl =
    let row_tmpl = Buffer.create 8 in
    Array.iteri
      (fun i (before, _) ->
         Buffer.add_string row_tmpl before;
         Buffer.add_string row_tmpl (String.make cell_widths.(i) ' '))
    cell_styles;
    Buffer.contents row_tmpl
  in
  let total_width = String.length row_tmpl in

  (* Title *)
  (match title with
   | None -> ()
   | Some title ->
     fpf fmt "%s@." (align_string Center (spf "╣ %s ╠" title) (total_width+4) ~fill:"═"));
  (* the +4 is a hack to compensate for the unicode characters in the centered string *)

  let pp_row row =
    (
      row |> List.iteri @@ fun i cell ->
      let (before, align) = cell_styles.(i) in
      let width = cell_widths.(i) in
      fpf fmt "%s%s" before (align_string align cell width)
    );
    fpf fmt "@."
  in

  let pp_sep n = (* n represents the position of the separator: top(-1), middle(0), bottom(1) *)
    let sep = Buffer.create 8 in
    String.iter
      (fun c ->
         Buffer.add_string
           sep
           (match c with
            | ' ' -> "—"
            | '|' ->
              if n < 0 then "┬"
              else if n = 0 then "┼"
              else "┴"
            | _ -> failwith ("unknown row template character: " ^ String.make 1 c)))
      row_tmpl;
    fpf fmt "%s@." (Buffer.contents sep)
  in

  pp_sep (-1);
  pp_row heading;
  pp_sep 0;
  List.iter pp_row content;
  pp_sep 1

let print_table ?title ~cell_styles ~heading t =
  pp_table ?title ~cell_styles ~heading Format.std_formatter t
