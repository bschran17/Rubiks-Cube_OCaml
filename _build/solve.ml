open Cube
open Set
module Stringmap = Map.Make (String)

type cross_find = (string * string list) Stringmap.t

let rec loop_cross_find
    (lst : (string * (string * string list)) list)
    map : cross_find =
  match lst with
  | [] -> map
  | (str, p) :: t -> loop_cross_find t (Stringmap.add str p map)

let make_cross_find (lst : (string * (string * string list)) list) =
  loop_cross_find lst Stringmap.empty

type t = string list

let solve cube = cube

let edge_list =
  [
    "p112";
    "p121";
    "p123";
    "p132";
    "p211";
    "p213";
    "p231";
    "p233";
    "p312";
    "p321";
    "p323";
    "p332";
  ]

let corner_list =
  [ "p111"; "p113"; "p131"; "p133"; "p311"; "p313"; "p331"; "p333" ]

let rec check_lsts
    (piece_colors : string list)
    (imp_colors : string list) : bool =
  match imp_colors with
  | h :: t ->
      if List.mem h piece_colors then check_lsts piece_colors t
      else false
  | [] -> true

let rec find_orient (lst : (string * string) list) (color : string) :
    string =
  match lst with
  | h :: t -> if fst h = color then snd h else find_orient t color
  | [] -> failwith "not a valid color set"

let rec find_correct_piece
    (cube : Cube.t)
    (colors : string list)
    (piece_type : string list)
    (piece_num : int) : string =
  let piece_str = List.nth piece_type piece_num in
  let piece = Cube.find_piece cube piece_str in
  let c_front = piece |> Piece.get_front |> Piece.string_of_color in
  let c_back = piece |> Piece.get_back |> Piece.string_of_color in
  let c_left = piece |> Piece.get_left |> Piece.string_of_color in
  let c_right = piece |> Piece.get_right |> Piece.string_of_color in
  let c_up = piece |> Piece.get_up |> Piece.string_of_color in
  let c_down = piece |> Piece.get_down |> Piece.string_of_color in
  let piece_colors =
    [ c_front; c_back; c_left; c_right; c_up; c_down ]
  in
  let () =
    Printf.printf "piece_colors: %s%s%s%s%s%s\n" c_front c_back c_left
      c_right c_up c_down
  in
  let color_map =
    [
      (c_front, "F");
      (c_back, "B");
      (c_right, "R");
      (c_left, "L");
      (c_up, "U");
      (c_down, "D");
    ]
  in
  let answer = check_lsts piece_colors colors in
  if answer then
    let top = List.hd colors in
    let orient = find_orient color_map top in
    piece_str ^ orient
  else find_correct_piece cube colors piece_type (piece_num + 1)

let white_blue_map =
  [
    ("p233U", ("p233U", [ "N" ]));
    ("p233B", ("p332B", [ "B'" ]));
    ("p332B", ("p323U", [ "R'" ]));
    ("p332R", ("p233U", [ "B" ]));
    ("p323U", ("p233U", [ "U'" ]));
    ("p323R", ("p332R", [ "R" ]));
    ("p112F", ("p123U", [ "L'" ]));
    ("p112L", ("p213U", [ "F" ]));
    ("p121L", ("p132L", [ "L" ]));
    ("p121D", ("p123U", [ "L2" ]));
    ("p123U", ("p233U", [ "U" ]));
    ("p123L", ("p132L", [ "L'" ]));
    ("p132B", ("p123U", [ "L" ]));
    ("p132L", ("p233U", [ "B'" ]));
    ("p211D", ("p231D", [ "D2" ]));
    ("p211F", ("p312F", [ "F'" ]));
    ("p213U", ("p233U", [ "U2" ]));
    ("p213F", ("p312F", [ "F" ]));
    ("p231D", ("p233U", [ "B2" ]));
    ("p231B", ("p321R", [ "D'" ]));
    ("p321R", ("p332R", [ "R'" ]));
    ("p321D", ("p231D", [ "D" ]));
    ("p312F", ("p323U", [ "R" ]));
    ("p312R", ("p213U", [ "F'" ]));
  ]

let white_orange_map =
  [
    ("p332B", ("p321D", [ "R" ]));
    ("p332R", ("p312R", [ "R2" ]));
    ("p323U", ("p321D", [ "R2" ]));
    ("p323R", ("p312R", [ "R'" ]));
    ("p112F", ("p123U", [ "L'" ]));
    ("p112L", ("p211D", [ "F'" ]));
    ("p121L", ("p112L", [ "L'" ]));
    ("p121D", ("p123U", [ "L2" ]));
    ("p123U", ("p123U", [ "N" ]));
    ("p123L", ("p112L", [ "L" ]));
    ("p132B", ("p123U", [ "L" ]));
    ("p132L", ("p112L", [ "L2" ]));
    ("p211D", ("p121D", [ "D'" ]));
    ("p211F", ("p112F", [ "F" ]));
    ("p213U", ("p211D", [ "F2" ]));
    ("p213F", ("p112F", [ "F'" ]));
    ("p231D", ("p121D", [ "D" ]));
    ("p231B", ("p121L", [ "D" ]));
    ("p321R", ("p211F", [ "D'" ]));
    ("p321D", ("p121D", [ "D2" ]));
    ("p312F", ("p112F", [ "F2" ]));
    ("p312R", ("p211D", [ "F" ]));
  ]

let white_green_map =
  [
    ("p332R", ("p312R", [ "R2" ]));
    ("p332B", ("p321D", [ "R" ]));
    ("p323U", ("p321D", [ "R2" ]));
    ("p323R", ("p312R", [ "R'" ]));
    ("p112L", ("p213U", [ "F" ]));
    ("p112F", ("p312F", [ "F2" ]));
    ("p121D", ("p211D", [ "D" ]));
    ("p121L", ("p321R", [ "D2" ]));
    ("p132B", ("p211D", [ "L'"; "D"; "L" ]));
    ("p132L", ("p211D", [ "B"; "D2"; "B'" ]));
    ("p211D", ("p213U", [ "F2" ]));
    ("p211F", ("p312F", [ "F'" ]));
    ("p213U", ("p213U", [ "N" ]));
    ("p213F", ("p312F", [ "F" ]));
    ("p231D", ("p211D", [ "D2" ]));
    ("p231B", ("p321R", [ "D'" ]));
    ("p321R", ("p312R", [ "R" ]));
    ("p321D", ("p211D", [ "D'" ]));
    ("p312F", ("p321D", [ "R'" ]));
    ("p312R", ("p213U", [ "F'" ]));
  ]

let white_red_map =
  [
    ("p332B", ("p323U", [ "R'" ]));
    ("p332R", ("p321D", [ "B'"; "D'"; "B" ]));
    ("p323U", ("p323U", [ "N" ]));
    ("p323R", ("p332R", [ "R" ]));
    ("p112F", ("p321D", [ "L"; "D2"; "L'" ]));
    ("p112L", ("p321D", [ "F'"; "D"; "F" ]));
    ("p121L", ("p211F", [ "D" ]));
    ("p121D", ("p321D", [ "D2" ]));
    ("p132B", ("p321D", [ "L'"; "D2"; "L" ]));
    ("p132L", ("p321D", [ "B"; "D'"; "B'" ]));
    ("p211D", ("p321D", [ "D" ]));
    ("p211F", ("p323U", [ "F'"; "R"; "F" ]));
    ("p231D", ("p321D", [ "D'" ]));
    ("p231B", ("p211F", [ "D2" ]));
    ("p321R", ("p211F", [ "D'" ]));
    ("p321D", ("p323U", [ "R2" ]));
    ("p312F", ("p323U", [ "R" ]));
    ("p312R", ("p321R", [ "R'" ]));
  ]

let rec get_turns or_pos to_pos turns map =
  if or_pos = to_pos then turns
  else
    try
      let tup = Stringmap.find or_pos map in
      get_turns (fst tup) to_pos (List.append turns (snd tup)) map
    with Not_found -> failwith "get turns doesn't work"

let make_cross cube =
  let pos1 = find_correct_piece cube [ "W"; "B" ] edge_list 0 in
  let () = Printf.printf "BluePiece that was found: %s\n" pos1 in
  let blue_white_map = make_cross_find white_blue_map in
  let turns = get_turns pos1 "p233U" [] blue_white_map in
  let updated = Cube.print_apply_turns turns cube in
  let pos2 = find_correct_piece updated [ "W"; "O" ] edge_list 0 in
  let () = Printf.printf "OrangePiece that was found: %s\n" pos2 in
  let orange_white_map = make_cross_find white_orange_map in
  let turns2 = get_turns pos2 "p123U" [] orange_white_map in
  let updated2 = Cube.print_apply_turns turns2 updated in
  let pos3 = find_correct_piece updated2 [ "W"; "G" ] edge_list 0 in
  let () = Printf.printf "GreenPiece that was found: %s\n" pos3 in
  let green_white_map = make_cross_find white_green_map in
  let turns3 = get_turns pos3 "p213U" [] green_white_map in
  let updated3 = Cube.print_apply_turns turns3 updated2 in
  let pos4 = find_correct_piece updated3 [ "W"; "R" ] edge_list 0 in
  let () = Printf.printf "RedPiece that was found: %s\n" pos4 in
  let red_white_map = make_cross_find white_red_map in
  let turns4 = get_turns pos4 "p323U" [] red_white_map in
  Cube.print_apply_turns turns4 updated3

let turn_top_layers cube = Cube.print_apply_turns [ "U"; "E" ] cube

let white_on_front cube =
  let cube1 = turn "clockwise" "F" cube in
  let cube2 = turn "clockwise" "D" cube1 in
  turn "counterclockwise" "F" cube2

(*need to append to list of turms somehow*)

let white_on_front cube = Cube.print_apply_turns [ "F"; "D"; "F'" ] cube

let white_on_side cube = Cube.print_apply_turns [ "R'"; "D'"; "R" ] cube

let rec no_conflict_cube cube =
  if Cube.get_p313 cube |> Piece.get_up |> Piece.string_of_color = "W"
  then no_conflict_cube (turn_top_layers cube)
  else cube

let white_on_bottom cube =
  let new_cube = no_conflict_cube cube in
  Cube.print_apply_turns [ "R'"; "D'"; "D'"; "R"; "D" ] new_cube

let white_top_right cube =
  Cube.print_apply_turns [ "R'"; "D'"; "R"; "D" ] cube

let white_top_front cube =
  Cube.print_apply_turns [ "F"; "D"; "F'"; "D'" ] cube

let incorrect_white_corner cube =
  Cube.print_apply_turns [ "R'"; "D'"; "R"; "D" ] cube

let turn_ccwise cube = Cube.print_apply_turns [ "D'" ] cube

let c_orient cube = Cube.print_apply_turns [ "U"; "E"; "D'" ] cube

(*pos is where the white sticker is*)
let rec find_white_corners cube corners_num turn_num =
  if corners_num < 4 then
    if turn_num < 4 then
      let p = Cube.get_p311 cube in
      let front = Piece.get_front p |> Piece.string_of_color in
      let right = Piece.get_right p |> Piece.string_of_color in
      let down = Piece.get_down p |> Piece.string_of_color in
      if front = "W" || right = "W" || down = "W" then
        place_white_corners cube p corners_num front right down
      else
        find_white_corners (turn_ccwise cube) corners_num (turn_num + 1)
    else check_top_corners cube corners_num 0
  else cube

and place_white_corners cube p corners_num front right down =
  if front = "W" then determine_orientation cube p corners_num "W" 0
  else if right = "W" then
    determine_orientation cube p corners_num "R" 0
  else if down = "W" then determine_orientation cube p corners_num "D" 0
  else failwith "IMPOSSIBLE"

(*side determines what side the white colored sticker is on*)
and determine_orientation cube piece corners_num side turn_num =
  if side = "F" then
    if
      Piece.get_right piece |> Piece.string_of_color
      = (Cube.get_p322 cube |> Piece.get_right |> Piece.string_of_color)
    then find_white_corners (white_on_front cube) (corners_num + 1) 0
    else if turn_num < 4 then
      determine_orientation (turn_top_layers cube) piece corners_num
        side (turn_num + 1)
    else find_white_corners cube corners_num 0
  else if side = "R" then
    if
      Piece.get_front piece |> Piece.string_of_color
      = (Cube.get_p212 cube |> Piece.get_front |> Piece.string_of_color)
    then find_white_corners (white_on_side cube) (corners_num + 1) 0
    else if turn_num < 4 then
      determine_orientation (turn_top_layers cube) piece corners_num
        side (turn_num + 1)
    else find_white_corners cube corners_num 0
  else find_white_corners (white_on_bottom cube) corners_num 0

and check_top_corners cube corners_num turn_num =
  if corners_num < 4 then
    if turn_num < 4 then
      let p = Cube.get_p313 cube in
      let front = Piece.get_front p |> Piece.string_of_color in
      let right = Piece.get_right p |> Piece.string_of_color in
      let up = Piece.get_up p |> Piece.string_of_color in
      if
        up = "W"
        && front
           = (Cube.get_p212 cube |> Piece.get_front
            |> Piece.string_of_color)
        && right
           = (Cube.get_p322 cube |> Piece.get_right
            |> Piece.string_of_color)
      then
        check_top_corners (turn_top_layers cube) corners_num
          (turn_num + 1)
      else if up = "W" then
        find_white_corners (incorrect_white_corner cube) corners_num 0
      else if up != "W" && right = "W" then
        get_white_out_of_top cube "R" corners_num
      else if up != "W" && front = "W" then
        get_white_out_of_top cube "F" corners_num
      else
        check_top_corners (turn_top_layers cube) corners_num
          (turn_num + 1)
    else cube
  else cube

and get_white_out_of_top cube pos corners_num =
  if pos = "R" then
    find_white_corners (white_top_right cube) corners_num 0
  else if pos = "F" then
    find_white_corners (white_top_front cube) corners_num 0
  else failwith "Impossible"

let rec fix_orientation cube turn_num =
  let front =
    Cube.get_p212 cube |> Piece.get_front |> Piece.string_of_color
  in
  let up =
    Cube.get_p223 cube |> Piece.get_up |> Piece.string_of_color
  in
  if front = "G" && up = "W" then cube
  else if turn_num < 4 then
    fix_orientation (c_orient cube) (turn_num + 1)
  else cube

let get_white_corners cube =
  let white_layer = find_white_corners cube 0 0 in
  fix_orientation white_layer 0

let cross_on_bottom cube =
  let cube = Cube.change_orientation "Z" "clockwise" cube in
  Cube.change_orientation "Z" "clockwise" cube

let rec fix_orientation cube =
  let center = find_piece cube "p212" in
  let center_color = get_color center "F" in
  if center_color = "G" then cube
  else
    let cube = Cube.change_orientation "Y" "clockwise" cube in
    fix_orientation cube

let check_completed_second_layer cube =
  let edge_1 = find_piece cube "p112" in
  let edge_2 = find_piece cube "p312" in
  let edge_3 = find_piece cube "p332" in
  let edge_4 = find_piece cube "p132" in
  let center_1 = find_piece cube "p212" in
  let center_2 = find_piece cube "p322" in
  let center_3 = find_piece cube "p232" in
  let center_4 = find_piece cube "p122" in
  let front_1 = get_color edge_1 "F" in
  let front_2 = get_color edge_2 "F" in
  let front_3 = get_color center_1 "F" in
  let right_1 = get_color edge_2 "R" in
  let right_2 = get_color edge_3 "R" in
  let right_3 = get_color center_2 "R" in
  let back_1 = get_color edge_3 "B" in
  let back_2 = get_color edge_4 "B" in
  let back_3 = get_color center_3 "B" in
  let left_1 = get_color edge_4 "L" in
  let left_2 = get_color edge_1 "L" in
  let left_3 = get_color center_4 "L" in
  if
    (front_1 = front_2 && front_2 = front_3)
    && (right_1 = right_2 && right_2 = right_3)
    && (back_1 = back_2 && back_2 = back_3)
    && left_1 = left_2 && left_2 = left_3
  then true
  else false

let rec check_top_edge cube counter =
  if check_completed_second_layer cube then cube
  else
    let piece = find_piece cube "p213" in
    let top_color = get_color piece "U" in
    let front_color = get_color piece "F" in
    if front_color = "Y" || top_color = "Y" then
      if counter = 3 then check_side_edge cube
      else
        let cube = Cube.print_apply_turns [ "U" ] cube in
        check_top_edge cube (counter + 1)
    else match_edge_with_face cube

and check_side_edge cube =
  let edge = find_piece cube "p312" in
  let center_f = find_piece cube "p212" in
  let center_r = find_piece cube "p322" in
  let edge_right = get_color edge "R" in
  let edge_front = get_color edge "F" in
  let center_front = get_color center_f "F" in
  let center_right = get_color center_r "R" in
  if edge_right = "Y" || edge_front = "Y" then
    let new_cube = Cube.change_orientation "Y" "clockwise" cube in
    check_side_edge new_cube
  else if edge_right = center_right && edge_front = center_front then
    let new_cube = Cube.change_orientation "Y" "clockwise" cube in
    check_side_edge new_cube
  else
    let cube =
      Cube.print_apply_turns
        [ "U"; "R"; "U'"; "R'"; "U'"; "F'"; "U"; "F" ]
        cube
    in
    check_top_edge cube 0

and place_edge_in_side cube =
  let edge = find_piece cube "p213" in
  let center = find_piece cube "p322" in
  let top_color = get_color edge "U" in
  let center_right_color = get_color center "R" in
  if top_color = center_right_color then
    let cube =
      Cube.print_apply_turns
        [ "U"; "R"; "U'"; "R'"; "U'"; "F'"; "U"; "F" ]
        cube
    in
    check_top_edge cube 0
  else
    let cube =
      Cube.print_apply_turns
        [ "U'"; "L'"; "U"; "L"; "U"; "F"; "U'"; "F'" ]
        cube
    in
    check_top_edge cube 0

and match_edge_with_face cube =
  let piece_edge = find_piece cube "p213" in
  let piece_center = find_piece cube "p212" in
  let edge_color = get_color piece_edge "F" in
  let center_color = get_color piece_center "F" in
  if edge_color = center_color then place_edge_in_side cube
  else
    let new_cube = Cube.change_orientation "Y" "clockwise" cube in
    let new_cube = Cube.print_apply_turns [ "U'" ] new_cube in
    match_edge_with_face new_cube

let solve_second_layer cube =
  let cube = cross_on_bottom cube in
  let cube = check_top_edge cube 0 in
  let cube = check_top_edge cube 0 in
  let cube = check_top_edge cube 0 in
  let cube = check_top_edge cube 0 in
  fix_orientation cube

let top_cross_alg cube =
  Cube.print_apply_turns [ "F"; "R"; "U"; "R'"; "U'"; "F'" ] cube

let rec check_edges (cube : Cube.t) cycle found color =
  if cycle = 4 then
    let dot_cube = top_cross_alg cube in
    check_edges dot_cube 0 0 "Y"
  else
    let p = Cube.find_piece cube "p213" in
    let top_sticker = p |> Piece.get_up |> Piece.string_of_color in
    if top_sticker = color then
      match found with
      | 0 -> check_edges (Cube.print_apply_turns [ "U" ] cube) 1 1 color
      | 1 when cycle = 1 ->
          check_edges (Cube.print_apply_turns [ "U" ] cube) 2 2 color
      | 1 when cycle = 2 ->
          top_cross_alg (Cube.print_apply_turns [ "U" ] cube)
      | 1 when cycle = 3 ->
          top_cross_alg
            (top_cross_alg (Cube.print_apply_turns [ "U2" ] cube))
      | 2 -> cube
    else
      let alt_cube = Cube.print_apply_turns [ "U" ] cube in
      check_edges alt_cube (cycle + 1) found color

let cross_on_top cube = check_edges cube 0 0 "Y"

let rec correct_spots lst num : int =
  match lst with
  | h :: t ->
      if fst h = snd h then correct_spots t (num + 1)
      else correct_spots t num
  | [] -> num

let set_up_move f b l r =
  match f with
  | true when r = true -> "U'"
  | true when l = true -> "U2"
  | true when b = true -> "N"
  | false when r = true && l = true -> "U"
  | false when r = true && b = true -> "N"
  | false when b = true && l = true -> "U"
  | _ -> failwith "loop with wrong in set_up_move"

let align_alg cube =
  Cube.print_apply_turns [ "R"; "U"; "R'"; "U"; "R"; "U2"; "R'" ] cube

let rec align cube =
  let pfront = Cube.find_piece cube "p213" in
  let pright = Cube.find_piece cube "p323" in
  let pleft = Cube.find_piece cube "p123" in
  let pback = Cube.find_piece cube "p233" in
  let front_sticker =
    pfront |> Piece.get_front |> Piece.string_of_color
  in
  let right_sticker =
    pright |> Piece.get_right |> Piece.string_of_color
  in
  let left_sticker = pleft |> Piece.get_left |> Piece.string_of_color in
  let back_sticker = pback |> Piece.get_back |> Piece.string_of_color in
  let sticker_tup =
    [
      (left_sticker, "R");
      (front_sticker, "G");
      (back_sticker, "B");
      (right_sticker, "O");
    ]
  in
  let num = correct_spots sticker_tup 0 in
  match num with
  | 1 | 0 -> align (Cube.print_apply_turns [ "U" ] cube)
  | 2 ->
      let fbool = "G" = front_sticker in
      let rbool = "O" = right_sticker in
      let lbool = "R" = left_sticker in
      let bbool = "B" = back_sticker in
      let set_up = set_up_move fbool bbool lbool rbool in
      align_alg (Cube.print_apply_turns [ set_up ] cube)
  | 4 -> cube
  | _ -> failwith "loop is wrong in align"

let rec last_adjustment cube =
  let p = Cube.find_piece cube "p213" in
  let front = p |> Piece.get_front |> Piece.string_of_color in
  match front with "G" -> cube | _ -> print_apply_turns [ "U" ] cube

let top_edges_align cube =
  let fixed = align cube in
  last_adjustment fixed

(*middle layer*)

let p313_lst cube =
  let p = Cube.get_p313 cube in
  let front = p |> Piece.get_front |> Piece.string_of_color in
  let right = p |> Piece.get_right |> Piece.string_of_color in
  let up = p |> Piece.get_up |> Piece.string_of_color in
  [ front; right; up ]

let centers_lst cube =
  let c_front =
    Cube.get_p212 cube |> Piece.get_front |> Piece.string_of_color
  in
  let c_right =
    Cube.get_p322 cube |> Piece.get_right |> Piece.string_of_color
  in
  let c_up =
    Cube.get_p223 cube |> Piece.get_up |> Piece.string_of_color
  in
  [ c_front; c_right; c_up ]

let rec is_match lst center_lst =
  match lst with
  | h :: t ->
      if List.mem h center_lst then is_match t center_lst else false
  | [] -> true

let is_p133_correct cube =
  let p = Cube.get_p133 cube in
  let up = p |> Piece.get_up |> Piece.string_of_color in
  let left = p |> Piece.get_left |> Piece.string_of_color in
  let back = p |> Piece.get_back |> Piece.string_of_color in
  let lst = [ up; left; back ] in
  let c_up =
    Cube.get_p223 cube |> Piece.get_up |> Piece.string_of_color
  in
  let c_left =
    Cube.get_p122 cube |> Piece.get_left |> Piece.string_of_color
  in
  let c_back =
    Cube.get_p232 cube |> Piece.get_back |> Piece.string_of_color
  in
  let c_lst = [ c_up; c_left; c_back ] in
  is_match lst c_lst

let is_p113_correct cube =
  let p = Cube.get_p113 cube in
  let up = p |> Piece.get_up |> Piece.string_of_color in
  let left = p |> Piece.get_left |> Piece.string_of_color in
  let front = p |> Piece.get_front |> Piece.string_of_color in
  let lst = [ up; left; front ] in
  let c_up =
    Cube.get_p223 cube |> Piece.get_up |> Piece.string_of_color
  in
  let c_left =
    Cube.get_p122 cube |> Piece.get_left |> Piece.string_of_color
  in
  let c_front =
    Cube.get_p212 cube |> Piece.get_front |> Piece.string_of_color
  in
  let c_lst = [ c_up; c_left; c_front ] in
  is_match lst c_lst

let is_p333_correct cube =
  let p = Cube.get_p333 cube in
  let up = p |> Piece.get_up |> Piece.string_of_color in
  let right = p |> Piece.get_right |> Piece.string_of_color in
  let back = p |> Piece.get_back |> Piece.string_of_color in
  let lst = [ up; right; back ] in
  let c_up =
    Cube.get_p223 cube |> Piece.get_up |> Piece.string_of_color
  in
  let c_right =
    Cube.get_p322 cube |> Piece.get_right |> Piece.string_of_color
  in
  let c_back =
    Cube.get_p232 cube |> Piece.get_back |> Piece.string_of_color
  in
  let c_lst = [ c_up; c_right; c_back ] in
  is_match lst c_lst

let turn_top cube = Cube.print_apply_turns [ "U" ] cube

let y_rotation cube = Cube.print_apply_turns [ "U"; "E"; "D'" ] cube

let rotate_piece cube =
  Cube.print_apply_turns
    [ "U"; "R"; "U'"; "L'"; "U"; "R'"; "U'"; "L" ]
    cube

let rec determine_correct cube corners_num turn_num =
  if corners_num < 1 then
    if turn_num < 4 then
      let center_lst = centers_lst cube in
      let lst = p313_lst cube in
      if is_match lst center_lst then
        check_other_corners cube (corners_num + 1) turn_num
      else determine_correct (y_rotation cube) corners_num (turn_num + 1)
    else determine_correct (rotate_piece cube) corners_num 0
  else cube

and check_other_corners cube corners_num turn_num =
  let is_p133 = is_p133_correct cube in
  let is_p113 = is_p113_correct cube in
  let is_p333 = is_p333_correct cube in

  if is_p133 && is_p113 && is_p333 then
    determine_correct cube 4 turn_num
  else check_other_corners (rotate_piece cube) corners_num turn_num

let rotate_corner cube =
  Cube.print_apply_turns
    [ "R'"; "D'"; "R"; "D"; "R'"; "D'"; "R"; "D" ]
    cube

let rec orient_corners cube corners_num =
  if corners_num < 4 then
    let p =
      Cube.get_p313 cube |> Piece.get_up |> Piece.string_of_color
    in
    let c =
      Cube.get_p223 cube |> Piece.get_up |> Piece.string_of_color
    in
    if p = c then orient_corners (turn_top cube) (corners_num + 1)
    else orient_corners (rotate_corner cube) corners_num
  else turn_top cube

let rec get_green_front cube =
  let c =
    Cube.get_p212 cube |> Piece.get_front |> Piece.string_of_color
  in
  if
    let p =
      Cube.get_p213 cube |> Piece.get_front |> Piece.string_of_color
    in
    c = p
  then if c = "G" then cube else get_green_front (y_rotation cube)
  else get_green_front (turn_top cube)

let check_yellow_corners cube =
  let corners_cube = determine_correct cube 0 0 in
  orient_corners corners_cube 0 |> get_green_front
