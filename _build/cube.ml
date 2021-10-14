open Piece
module Rubiks = Map.Make (String)

exception InvalidPiece

exception InvalidOrientation

exception InvalidFace

type t = Piece.t Rubiks.t

(**[find_piece cube piece] is the [Piece.t] bound to [piece] in [cube]*)
let find_piece cube (piece : string) =
  try Rubiks.find piece cube
  with Not_found -> failwith "Not a valid piece"

(**[change_cube lst cube] adds the bindings in lst, a
   [(string * Piece.t) list] to [cube]. It uses the OCaml [Map.add], so
   old bindings with the same key are removed*)
let rec change_cube (lst : (string * Piece.t) list) cube =
  match lst with
  | [] -> cube
  | (str, p) :: t -> change_cube t (Rubiks.add str p cube)

let make_cube (lst : (string * Piece.t) list) =
  change_cube lst Rubiks.empty

let get_color (piece : Piece.t) (orientation : string) =
  match orientation with
  | "F" -> Piece.get_front piece |> Piece.string_of_color
  | "B" -> Piece.get_back piece |> Piece.string_of_color
  | "L" -> Piece.get_left piece |> Piece.string_of_color
  | "R" -> Piece.get_right piece |> Piece.string_of_color
  | "U" -> Piece.get_up piece |> Piece.string_of_color
  | "D" -> Piece.get_down piece |> Piece.string_of_color
  | _ -> raise InvalidOrientation

let get_print_color = function
  | "R" -> ANSITerminal.print_string [ ANSITerminal.on_black ] "🟥"
  | "B" -> ANSITerminal.print_string [ ANSITerminal.on_black ] "🟦"
  | "W" -> ANSITerminal.print_string [ ANSITerminal.on_black ] "⬜️"
  | "Y" -> ANSITerminal.print_string [ ANSITerminal.on_black ] "🟨"
  | "G" -> ANSITerminal.print_string [ ANSITerminal.on_black ] "🟩"
  | "O" -> ANSITerminal.print_string [ ANSITerminal.on_black ] "🟧"
  | _ -> failwith "not a correct color"

let rec actual_print_row row ori =
  match row with
  | [] -> print_string ""
  | h :: t ->
      let color1 = get_color h ori in
      get_print_color color1;
      actual_print_row t ori

let get_pieces_lst lst cube =
  let rec get_pieces_r lst acc =
    match lst with
    | [] -> List.rev acc
    | h :: t -> get_pieces_r t (find_piece cube h :: acc)
  in
  get_pieces_r lst []

let print_row cube face row =
  let row =
    match face with
    | "F" when row = 1 -> get_pieces_lst [ "p111"; "p211"; "p311" ] cube
    | "F" when row = 2 -> get_pieces_lst [ "p112"; "p212"; "p312" ] cube
    | "F" when row = 3 -> get_pieces_lst [ "p113"; "p213"; "p313" ] cube
    | "B" when row = 1 -> get_pieces_lst [ "p331"; "p231"; "p131" ] cube
    | "B" when row = 2 -> get_pieces_lst [ "p332"; "p232"; "p132" ] cube
    | "B" when row = 3 -> get_pieces_lst [ "p333"; "p233"; "p133" ] cube
    | "L" when row = 1 -> get_pieces_lst [ "p131"; "p121"; "p111" ] cube
    | "L" when row = 2 -> get_pieces_lst [ "p132"; "p122"; "p112" ] cube
    | "L" when row = 3 -> get_pieces_lst [ "p133"; "p123"; "p113" ] cube
    | "R" when row = 1 -> get_pieces_lst [ "p311"; "p321"; "p331" ] cube
    | "R" when row = 2 -> get_pieces_lst [ "p312"; "p322"; "p332" ] cube
    | "R" when row = 3 -> get_pieces_lst [ "p313"; "p323"; "p333" ] cube
    | "U" when row = 1 -> get_pieces_lst [ "p113"; "p213"; "p313" ] cube
    | "U" when row = 2 -> get_pieces_lst [ "p123"; "p223"; "p323" ] cube
    | "U" when row = 3 -> get_pieces_lst [ "p133"; "p233"; "p333" ] cube
    | "D" when row = 1 -> get_pieces_lst [ "p131"; "p231"; "p331" ] cube
    | "D" when row = 2 -> get_pieces_lst [ "p121"; "p221"; "p321" ] cube
    | "D" when row = 3 -> get_pieces_lst [ "p111"; "p211"; "p311" ] cube
    | _ -> failwith "Invalid face"
  in
  actual_print_row row face

let print_row_back_unfolded cube row =
  let row =
    match row with
    | 1 -> get_pieces_lst [ "p133"; "p233"; "p333" ] cube
    | 2 -> get_pieces_lst [ "p132"; "p232"; "p332" ] cube
    | 3 -> get_pieces_lst [ "p131"; "p231"; "p331" ] cube
    | _ -> failwith "Invalid row"
  in
  actual_print_row row "B"

let print_face cube face =
  print_row cube face 3;
  print_endline "";
  print_row cube face 2;
  print_endline "";
  print_row cube face 1;
  print_endline "";
  ()

let print_all_faces cube =
  print_endline "Top face:";
  print_face cube "U";

  print_endline "Bottom face:";
  print_face cube "D";

  print_endline "Left face:";
  print_face cube "L";

  print_endline "Right face:";
  print_face cube "R";

  print_endline "Front face:";
  print_face cube "F";

  print_endline "Back face:";
  print_face cube "B";
  ()

(**[new_line_indt ()] prints a new line to the terminal, and then
   indents*)
let new_line_indt () =
  print_endline "";
  print_string "      "

let print_unfolded cube =
  print_string "      ";
  print_row cube "U" 3;
  new_line_indt ();
  print_row cube "U" 2;
  new_line_indt ();
  print_row cube "U" 1;
  print_endline "";
  print_row cube "L" 3;
  print_row cube "F" 3;
  print_row cube "R" 3;
  print_endline "";
  print_row cube "L" 2;
  print_row cube "F" 2;
  print_row cube "R" 2;
  print_endline "";
  print_row cube "L" 1;
  print_row cube "F" 1;
  print_row cube "R" 1;
  new_line_indt ();
  print_row cube "D" 3;
  new_line_indt ();
  print_row cube "D" 2;
  new_line_indt ();
  print_row cube "D" 1;
  new_line_indt ();
  print_row_back_unfolded cube 3;
  new_line_indt ();
  print_row_back_unfolded cube 2;
  new_line_indt ();
  print_row_back_unfolded cube 1;
  print_endline "";
  ()

let make_solved_lst =
  [
    ("p111", Piece.make_piece "G" "" "" "O" "" "Y");
    ("p112", Piece.make_piece "G" "" "" "O" "" "");
    ("p113", Piece.make_piece "G" "" "" "O" "W" "");
    ("p121", Piece.make_piece "" "" "" "O" "" "Y");
    ("p122", Piece.make_piece "" "" "" "O" "" "");
    ("p123", Piece.make_piece "" "" "" "O" "W" "");
    ("p131", Piece.make_piece "" "B" "" "O" "" "Y");
    ("p132", Piece.make_piece "" "B" "" "O" "" "");
    ("p133", Piece.make_piece "" "B" "" "O" "W" "");
    ("p211", Piece.make_piece "G" "" "" "" "" "Y");
    ("p212", Piece.make_piece "G" "" "" "" "" "");
    ("p213", Piece.make_piece "G" "" "" "" "W" "");
    ("p221", Piece.make_piece "" "" "" "" "" "Y");
    ("p223", Piece.make_piece "" "" "" "" "W" "");
    ("p231", Piece.make_piece "" "B" "" "" "" "Y");
    ("p232", Piece.make_piece "" "B" "" "" "" "");
    ("p233", Piece.make_piece "" "B" "" "" "W" "");
    ("p311", Piece.make_piece "G" "" "R" "" "" "Y");
    ("p312", Piece.make_piece "G" "" "R" "" "" "");
    ("p313", Piece.make_piece "G" "" "R" "" "W" "");
    ("p321", Piece.make_piece "" "" "R" "" "" "Y");
    ("p322", Piece.make_piece "" "" "R" "" "" "");
    ("p323", Piece.make_piece "" "" "R" "" "W" "");
    ("p331", Piece.make_piece "" "B" "R" "" "" "Y");
    ("p332", Piece.make_piece "" "B" "R" "" "" "");
    ("p333", Piece.make_piece "" "B" "R" "" "W" "");
  ]

let make_scram_lst =
  [
    ("p111", Piece.make_piece "G" "" "" "R" "" "W");
    ("p112", Piece.make_piece "B" "" "" "W" "" "");
    ("p113", Piece.make_piece "R" "" "" "B" "Y" "");
    ("p121", Piece.make_piece "" "" "" "O" "" "G");
    ("p122", Piece.make_piece "" "" "" "G" "" "");
    ("p123", Piece.make_piece "" "" "" "Y" "R" "");
    ("p131", Piece.make_piece "" "W" "" "B" "" "R");
    ("p132", Piece.make_piece "" "R" "" "G" "" "");
    ("p133", Piece.make_piece "" "Y" "" "R" "G" "");
    ("p211", Piece.make_piece "W" "" "" "" "" "G");
    ("p212", Piece.make_piece "Y" "" "" "" "" "");
    ("p213", Piece.make_piece "O" "" "" "" "B" "");
    ("p221", Piece.make_piece "" "" "" "" "" "O");
    ("p223", Piece.make_piece "" "" "" "" "R" "");
    ("p231", Piece.make_piece "" "G" "" "" "" "Y");
    ("p232", Piece.make_piece "" "W" "" "" "" "");
    ("p233", Piece.make_piece "" "B" "" "" "Y" "");
    ("p311", Piece.make_piece "G" "" "O" "" "" "W");
    ("p312", Piece.make_piece "W" "" "O" "" "" "");
    ("p313", Piece.make_piece "B" "" "Y" "" "O" "");
    ("p321", Piece.make_piece "" "" "B" "" "" "R");
    ("p322", Piece.make_piece "" "" "B" "" "" "");
    ("p323", Piece.make_piece "" "" "O" "" "Y" "");
    ("p331", Piece.make_piece "" "O" "Y" "" "" "G");
    ("p332", Piece.make_piece "" "R" "W" "" "" "");
    ("p333", Piece.make_piece "" "W" "O" "" "B" "");
  ]

let get_p111 cube = find_piece cube "p111"

let get_p112 cube = find_piece cube "p112"

let get_p113 cube = find_piece cube "p113"

let get_p211 cube = find_piece cube "p211"

let get_p212 cube = find_piece cube "p212"

let get_p213 cube = find_piece cube "p213"

let get_p311 cube = find_piece cube "p311"

let get_p312 cube = find_piece cube "p312"

let get_p313 cube = find_piece cube "p313"

let get_p121 cube = find_piece cube "p121"

let get_p122 cube = find_piece cube "p122"

let get_p123 cube = find_piece cube "p123"

let get_p221 cube = find_piece cube "p221"

let get_p223 cube = find_piece cube "p223"

let get_p321 cube = find_piece cube "p321"

let get_p322 cube = find_piece cube "p322"

let get_p323 cube = find_piece cube "p323"

let get_p131 cube = find_piece cube "p131"

let get_p132 cube = find_piece cube "p132"

let get_p133 cube = find_piece cube "p133"

let get_p231 cube = find_piece cube "p231"

let get_p232 cube = find_piece cube "p232"

let get_p233 cube = find_piece cube "p233"

let get_p331 cube = find_piece cube "p331"

let get_p332 cube = find_piece cube "p332"

let get_p333 cube = find_piece cube "p333"

let get_piece (cube : t) (piece : string) = Rubiks.find piece cube

(**[match_face str] returns the get function that corresponds to the
   string [str]*)
let match_face = function
  | "front" -> get_front
  | "back" -> get_back
  | "down" -> get_down
  | "up" -> get_up
  | "left" -> get_left
  | "right" -> get_right
  | _ -> failwith "Impossible"

(**[make_piece_lst lst piece] makes a list of the colors of [piece]
   according to the specified faces in [lst]*)
let make_piece_lst lst piece =
  let rec make_lst_r lst' acc =
    match lst' with
    | [] -> List.rev acc
    | h :: t -> make_lst_r t ((match_face h) piece :: acc)
  in
  make_lst_r lst []

(**[oriented_list face piece] is the the list of colors of [piece]
   reordered to represent the new order they will be stored in when a
   new piece is made. This order is front, back, left, right, up, down*)
let oriented_list (face : string) (piece : Piece.t) =
  match face with
  | "F" ->
      make_piece_lst
        [ "front"; "back"; "down"; "up"; "left"; "right" ]
        piece
  | "B" ->
      make_piece_lst
        [ "front"; "back"; "up"; "down"; "right"; "left" ]
        piece
  | "L" ->
      make_piece_lst
        [ "up"; "down"; "left"; "right"; "back"; "front" ]
        piece
  | "R" ->
      make_piece_lst
        [ "down"; "up"; "left"; "right"; "front"; "back" ]
        piece
  | "U" ->
      make_piece_lst
        [ "right"; "left"; "front"; "back"; "up"; "down" ]
        piece
  | "D" ->
      make_piece_lst
        [ "left"; "right"; "back"; "front"; "up"; "down" ]
        piece
  | _ -> failwith "Invalid face"

(**[make_new_piece piece face] reorients [piece] to carry out a turn
   when done from [face]. This is done in a specific pattern*)
let make_new_piece (piece : Piece.t) (face : string) =
  let lst = oriented_list face piece in
  match lst with
  | f :: b :: l :: r :: u :: d :: empty ->
      make_piece (string_of_color f) (string_of_color b)
        (string_of_color r) (string_of_color l) (string_of_color u)
        (string_of_color d)
  | _ -> failwith "Invalid"

(**[make_update_lst lst cube dir] makes the [string * Piece.t] lst which
   represents the new bindings to be added to [cube] to update it for a
   turn*)
let make_update_lst (lst : (string * string) list) cube dir =
  let rec update p_list update_lst =
    match p_list with
    | [] -> update_lst
    | (new_loc, old_p) :: t ->
        update t
          ((new_loc, make_new_piece (find_piece cube old_p) dir)
          :: update_lst)
  in
  update lst []

let turn_right cube =
  let lst =
    [
      ("p313", "p311");
      ("p323", "p312");
      ("p333", "p313");
      ("p312", "p321");
      ("p332", "p323");
      ("p311", "p331");
      ("p321", "p332");
      ("p331", "p333");
    ]
  in
  change_cube (make_update_lst lst cube "R") cube

let turn_left cube =
  let lst =
    [
      ("p113", "p133");
      ("p123", "p132");
      ("p133", "p131");
      ("p112", "p123");
      ("p132", "p121");
      ("p111", "p113");
      ("p121", "p112");
      ("p131", "p111");
    ]
  in
  change_cube (make_update_lst lst cube "L") cube

let turn_top cube =
  let lst =
    [
      ("p113", "p313");
      ("p123", "p213");
      ("p133", "p113");
      ("p213", "p323");
      ("p233", "p123");
      ("p313", "p333");
      ("p323", "p233");
      ("p333", "p133");
    ]
  in
  change_cube (make_update_lst lst cube "U") cube

let turn_bottom cube =
  let lst =
    [
      ("p111", "p131");
      ("p121", "p231");
      ("p131", "p331");
      ("p211", "p121");
      ("p231", "p321");
      ("p311", "p111");
      ("p321", "p211");
      ("p331", "p311");
    ]
  in
  change_cube (make_update_lst lst cube "D") cube

let turn_front cube =
  let lst =
    [
      ("p113", "p111");
      ("p213", "p112");
      ("p313", "p113");
      ("p112", "p211");
      ("p312", "p213");
      ("p111", "p311");
      ("p211", "p312");
      ("p311", "p313");
    ]
  in
  change_cube (make_update_lst lst cube "F") cube

let turn_back cube =
  let lst =
    [
      ("p133", "p333");
      ("p233", "p332");
      ("p333", "p331");
      ("p132", "p233");
      ("p131", "p133");
      ("p231", "p132");
      ("p331", "p131");
      ("p332", "p231");
    ]
  in
  change_cube (make_update_lst lst cube "B") cube

let m_slice cube =
  let lst =
    [
      ("p213", "p211");
      ("p223", "p212");
      ("p233", "p213");
      ("p212", "p221");
      ("p232", "p223");
      ("p211", "p231");
      ("p221", "p232");
      ("p231", "p233");
    ]
  in
  change_cube (make_update_lst lst cube "R") cube

let e_slice cube =
  let lst =
    [
      ("p112", "p312");
      ("p122", "p212");
      ("p132", "p112");
      ("p212", "p322");
      ("p232", "p122");
      ("p312", "p332");
      ("p322", "p232");
      ("p332", "p132");
    ]
  in
  change_cube (make_update_lst lst cube "U") cube

let s_slice cube =
  let lst =
    [
      ("p123", "p121");
      ("p223", "p122");
      ("p323", "p123");
      ("p122", "p221");
      ("p322", "p223");
      ("p121", "p321");
      ("p221", "p322");
      ("p321", "p323");
    ]
  in
  change_cube (make_update_lst lst cube "F") cube

let turn direction face cube =
  match face with
  | "R" when direction = "clockwise" -> turn_right cube
  | "R" -> cube |> turn_right |> turn_right |> turn_right
  | "L" when direction = "clockwise" -> turn_left cube
  | "L" -> cube |> turn_left |> turn_left |> turn_left
  | "U" when direction = "clockwise" -> turn_top cube
  | "U" -> cube |> turn_top |> turn_top |> turn_top
  | "D" when direction = "clockwise" -> turn_bottom cube
  | "D" -> cube |> turn_bottom |> turn_bottom |> turn_bottom
  | "F" when direction = "clockwise" -> turn_front cube
  | "F" -> cube |> turn_front |> turn_front |> turn_front
  | "B" when direction = "clockwise" -> turn_back cube
  | "B" -> cube |> turn_back |> turn_back |> turn_back
  | "M" when direction = "clockwise" -> m_slice cube
  | "M" -> cube |> m_slice |> m_slice |> m_slice
  | "E" when direction = "clockwise" -> e_slice cube
  | "E" -> cube |> e_slice |> e_slice |> e_slice
  | "S" when direction = "clockwise" -> s_slice cube
  | "S" -> cube |> s_slice |> s_slice |> s_slice
  | _ -> raise InvalidOrientation

let change_orientation axis direction cube =
  match axis with
  | "X" ->
      let cube = turn direction "R" cube in
      let cube = turn direction "M" cube in
      if direction = "clockwise" then turn "counterclockwise" "L" cube
      else turn "clockwise" "L" cube
  | "Y" ->
      let cube = turn direction "U" cube in
      let cube = turn direction "E" cube in
      if direction = "clockwise" then turn "counterclockwise" "D" cube
      else turn "clockwise" "D" cube
  | "Z" ->
      let cube = turn direction "F" cube in
      let cube = turn direction "S" cube in
      if direction = "clockwise" then turn "counterclockwise" "B" cube
      else turn "clockwise" "B" cube
  | _ -> failwith "Invalid axis"

(**[parse_turn str cube] is the resulting cube after the turn [str] is
   applied to [cube]. ["N"] represents no move. Requires: [str] is a
   valid input that represents a move, in cube notation. *)
let parse_turn str cube =
  match str with
  | "N" -> cube
  | "R" -> turn "clockwise" "R" cube
  | "R'" -> turn "counterclockwise" "R" cube
  | "R2" -> cube |> turn "clockwise" "R" |> turn "clockwise" "R"
  | "L" -> turn "clockwise" "L" cube
  | "L'" -> turn "counterclockwise" "L" cube
  | "L2" -> cube |> turn "clockwise" "L" |> turn "clockwise" "L"
  | "U" -> turn "clockwise" "U" cube
  | "U'" -> turn "counterclockwise" "U" cube
  | "U2" -> cube |> turn "clockwise" "U" |> turn "clockwise" "U"
  | "D" -> turn "clockwise" "D" cube
  | "D'" -> turn "counterclockwise" "D" cube
  | "D2" -> cube |> turn "clockwise" "D" |> turn "clockwise" "D"
  | "B" -> turn "clockwise" "B" cube
  | "B'" -> turn "counterclockwise" "B" cube
  | "B2" -> cube |> turn "clockwise" "B" |> turn "clockwise" "B"
  | "F" -> turn "clockwise" "F" cube
  | "F'" -> turn "counterclockwise" "F" cube
  | "F2" -> cube |> turn "clockwise" "F" |> turn "clockwise" "F"
  | "M" -> turn "clockwise" "M" cube
  | "M'" -> turn "counterclockwise" "M" cube
  | "M2" -> cube |> turn "clockwise" "M" |> turn "clockwise" "M"
  | "E" -> turn "clockwise" "E" cube
  | "E'" -> turn "counterclockwise" "E" cube
  | "E2" -> cube |> turn "clockwise" "E" |> turn "clockwise" "E"
  | "S" -> turn "clockwise" "S" cube
  | "S'" -> turn "counterclockwise" "S" cube
  | "S2" -> cube |> turn "clockwise" "S" |> turn "clockwise" "S"
  | _ -> failwith "Impossible"

let rec print_apply_turns lst cube =
  match lst with
  | [] -> cube
  | h :: t ->
      let new_cube = parse_turn h cube in
      if h <> "N" then (
        print_endline (h ^ ":");
        print_unfolded new_cube)
      else ();
      print_apply_turns t new_cube
