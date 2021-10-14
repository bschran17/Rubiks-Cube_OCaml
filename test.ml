(**Test Plan:

   - Automatic Testing (using OUnit): Creating pieces; Creating of cube
     (pieces have correct orientation and are in correct location on
     cube); All turning functions (All faces, both clockwise and
     counterclockwise); Functions used for the solving algorithm

   - Manual Testing: Printing functions (unfolded and printing by face);
     User interface when running [make play]

   The [Cube], [Piece], and [Solve] modules were tested using OUnit
   testing. They were tested using black-box testing, as only their
   output was looked at. The [Main] module was not tested using OUnit
   and was instead tested manually by looking at the terminal output

   The testing described above shows that the system behaves correctly.
   Because the tests show that pieces are created and accessed
   correctly, [Piece] is considered correct. Then because those pieces
   are used to make a cube, which is tested to see if pieces are in the
   correct location after a cube is made with all piceces, the cube is
   shown to be created correctly. By testing each type of turn on each
   face in each direction, it is shown that turns behave correctly. This
   means that the cube can act like a function real-life Rubik's cube.
   Then, testing that each function that represents a major step in the
   solving algorithm in [Solve] gets multiple configurations of a cube
   to the desired point shows that the solving algorithm can
   successfully get any cube closer to the solved state. Then, when
   combined together, these functions will be able to solve any Rubik's
   cube. *)
open Cube

open Solve
open Piece
open OUnit2
open Random

let unit_piece = make_piece "R" "O" "Y" "G" "B" "W"

let solved_cube = make_cube make_solved_lst

let piece_rep_test name dir piece expected =
  name >:: fun _ ->
  assert_equal expected
    (piece |> dir |> string_of_color)
    ~printer:(fun x -> x)

let piece_color_test name dir piece cube expected =
  name >:: fun _ ->
  assert_equal expected
    (find_piece cube piece |> dir |> string_of_color)
    ~printer:(fun x -> x)

let representation_tests =
  [
    (*testing represention of a 1x1 unit cube: *)
    piece_rep_test "Front face on unit piece" get_front unit_piece "R";
    piece_rep_test "Back face on unit piece" get_back unit_piece "O";
    piece_rep_test "Right face on unit piece" get_right unit_piece "Y";
    piece_rep_test "Left face on unit piece" get_left unit_piece "G";
    piece_rep_test "Top face on unit piece" get_up unit_piece "B";
    piece_rep_test "Bottom face on unit piece" get_down unit_piece "W";
    (*testing representation for 3x3 Rubik's cube: *)
    (*front face: *)
    piece_color_test "front p111" get_front "p111" solved_cube "G";
    piece_color_test "front p211" get_front "p211" solved_cube "G";
    piece_color_test "front p311" get_front "p311" solved_cube "G";
    piece_color_test "front p112" get_front "p112" solved_cube "G";
    piece_color_test "front p212" get_front "p212" solved_cube "G";
    piece_color_test "front p312" get_front "p312" solved_cube "G";
    piece_color_test "front p113" get_front "p113" solved_cube "G";
    piece_color_test "front p213" get_front "p213" solved_cube "G";
    piece_color_test "front p313" get_front "p313" solved_cube "G";
    (*back face: *)
    piece_color_test "back p111" get_back "p131" solved_cube "B";
    piece_color_test "back p211" get_back "p231" solved_cube "B";
    piece_color_test "back p311" get_back "p331" solved_cube "B";
    piece_color_test "back p112" get_back "p132" solved_cube "B";
    piece_color_test "back p212" get_back "p232" solved_cube "B";
    piece_color_test "back p312" get_back "p332" solved_cube "B";
    piece_color_test "back p113" get_back "p133" solved_cube "B";
    piece_color_test "back p213" get_back "p233" solved_cube "B";
    piece_color_test "back p313" get_back "p333" solved_cube "B";
    (*right face: *)
    piece_color_test "right p311" get_right "p311" solved_cube "R";
    piece_color_test "right p321" get_right "p321" solved_cube "R";
    piece_color_test "right p331" get_right "p331" solved_cube "R";
    piece_color_test "right p312" get_right "p312" solved_cube "R";
    piece_color_test "right p322" get_right "p322" solved_cube "R";
    piece_color_test "right p332" get_right "p332" solved_cube "R";
    piece_color_test "right p313" get_right "p313" solved_cube "R";
    piece_color_test "right p323" get_right "p323" solved_cube "R";
    piece_color_test "right p333" get_right "p333" solved_cube "R";
    (*left face: *)
    piece_color_test "left p311" get_left "p111" solved_cube "O";
    piece_color_test "left p321" get_left "p121" solved_cube "O";
    piece_color_test "left p331" get_left "p131" solved_cube "O";
    piece_color_test "left p312" get_left "p112" solved_cube "O";
    piece_color_test "left p322" get_left "p122" solved_cube "O";
    piece_color_test "left p332" get_left "p132" solved_cube "O";
    piece_color_test "left p313" get_left "p113" solved_cube "O";
    piece_color_test "left p323" get_left "p123" solved_cube "O";
    piece_color_test "left p333" get_left "p133" solved_cube "O";
    (*top face: *)
    piece_color_test "top p113" get_up "p113" solved_cube "W";
    piece_color_test "top p213" get_up "p213" solved_cube "W";
    piece_color_test "top p313" get_up "p313" solved_cube "W";
    piece_color_test "top p123" get_up "p123" solved_cube "W";
    piece_color_test "top p223" get_up "p223" solved_cube "W";
    piece_color_test "top p323" get_up "p323" solved_cube "W";
    piece_color_test "top p133" get_up "p113" solved_cube "W";
    piece_color_test "top p233" get_up "p233" solved_cube "W";
    piece_color_test "top p333" get_up "p333" solved_cube "W";
    (*back face: *)
    piece_color_test "bottom p113" get_down "p111" solved_cube "Y";
    piece_color_test "bottom p213" get_down "p211" solved_cube "Y";
    piece_color_test "bottom p213" get_down "p211" solved_cube "Y";
    piece_color_test "bottom p313" get_down "p311" solved_cube "Y";
    piece_color_test "bottom p123" get_down "p121" solved_cube "Y";
    piece_color_test "bottom p223" get_down "p221" solved_cube "Y";
    piece_color_test "bottom p323" get_down "p321" solved_cube "Y";
    piece_color_test "bottom p133" get_down "p111" solved_cube "Y";
    piece_color_test "bottom p233" get_down "p231" solved_cube "Y";
    piece_color_test "bottom p333" get_down "p331" solved_cube "Y";
  ]

let scrambled_cube = make_cube make_scram_lst

let print_piece piece =
  "\nFront: "
  ^ (piece |> Piece.get_front |> Piece.string_of_color)
  ^ "\nBack: "
  ^ (piece |> Piece.get_back |> Piece.string_of_color)
  ^ "\nLeft: "
  ^ (piece |> Piece.get_left |> Piece.string_of_color)
  ^ "\nRight: "
  ^ (piece |> Piece.get_right |> Piece.string_of_color)
  ^ "\nTop: "
  ^ (piece |> Piece.get_up |> Piece.string_of_color)
  ^ "\nDown: "
  ^ (piece |> Piece.get_down |> Piece.string_of_color)
  ^ "\n"

let turn_piece_test name piece cube face direction f b r l u d =
  name >:: fun _ ->
  assert_equal
    (make_piece f b r l u d)
    (find_piece (Cube.turn direction face cube) piece)
    ~printer:print_piece

let turn_tests =
  [
    turn_piece_test "Solved front p111" "p111" solved_cube "F"
      "clockwise" "G" "" "" "Y" "" "R";
    turn_piece_test "Solved front p112" "p112" solved_cube "F"
      "clockwise" "G" "" "" "Y" "" "";
    turn_piece_test "Solved front p113" "p113" solved_cube "F"
      "clockwise" "G" "" "" "Y" "O" "";
    turn_piece_test "Solved front p211" "p211" solved_cube "F"
      "clockwise" "G" "" "" "" "" "R";
    turn_piece_test "Solved front p213" "p213" solved_cube "F"
      "clockwise" "G" "" "" "" "O" "";
    turn_piece_test "Solved front p311" "p311" solved_cube "F"
      "clockwise" "G" "" "W" "" "" "R";
    turn_piece_test "Solved front p312" "p312" solved_cube "F"
      "clockwise" "G" "" "W" "" "" "";
    turn_piece_test "Solved front p313" "p313" solved_cube "F"
      "clockwise" "G" "" "W" "" "O" "";
    turn_piece_test "Solved left p131" "p131" solved_cube "L"
      "clockwise" "" "Y" "" "O" "" "G";
    turn_piece_test "Solved left p121" "p121" solved_cube "L"
      "clockwise" "" "" "" "O" "" "G";
    turn_piece_test "Solved left p111" "p111" solved_cube "L"
      "clockwise" "W" "" "" "O" "" "G";
    turn_piece_test "Solved left p132" "p132" solved_cube "L"
      "clockwise" "" "Y" "" "O" "" "";
    turn_piece_test "Solved left p112" "p112" solved_cube "L"
      "clockwise" "W" "" "" "O" "" "";
    turn_piece_test "Solved left p133" "p133" solved_cube "L"
      "clockwise" "" "Y" "" "O" "B" "";
    turn_piece_test "Solved left p123" "p123" solved_cube "L"
      "clockwise" "" "" "" "O" "B" "";
    turn_piece_test "Solved left p113" "p113" solved_cube "L"
      "clockwise" "W" "" "" "O" "B" "";
    turn_piece_test "Solved bottom p111" "p111" solved_cube "D"
      "clockwise" "O" "" "" "B" "" "Y";
    turn_piece_test "Solved bottom p211" "p211" solved_cube "D"
      "clockwise" "O" "" "" "" "" "Y";
    turn_piece_test "Solved bottom p311" "p311" solved_cube "D"
      "clockwise" "O" "" "G" "" "" "Y";
    turn_piece_test "Solved bottom p121" "p121" solved_cube "D"
      "clockwise" "" "" "" "B" "" "Y";
    turn_piece_test "Solved bottom p321" "p321" solved_cube "D"
      "clockwise" "" "" "G" "" "" "Y";
    turn_piece_test "Solved bottom p131" "p131" solved_cube "D"
      "clockwise" "" "R" "" "B" "" "Y";
    turn_piece_test "Solved bottom p231" "p231" solved_cube "D"
      "clockwise" "" "R" "" "" "" "Y";
    turn_piece_test "Solved bottom p331" "p331" solved_cube "D"
      "clockwise" "" "R" "G" "" "" "Y";
    (*Counterclockwise tests: *)
    turn_piece_test "Solved right p311" "p311" solved_cube "R"
      "counterclockwise" "W" "" "R" "" "" "G";
    turn_piece_test "Solved right p321" "p321" solved_cube "R"
      "counterclockwise" "" "" "R" "" "" "G";
    turn_piece_test "Solved right p331" "p331" solved_cube "R"
      "counterclockwise" "" "Y" "R" "" "" "G";
    turn_piece_test "Solved right p312" "p312" solved_cube "R"
      "counterclockwise" "W" "" "R" "" "" "";
    turn_piece_test "Solved right p332" "p332" solved_cube "R"
      "counterclockwise" "" "Y" "R" "" "" "";
    turn_piece_test "Solved right p313" "p313" solved_cube "R"
      "counterclockwise" "W" "" "R" "" "B" "";
    turn_piece_test "Solved right p323" "p323" solved_cube "R"
      "counterclockwise" "" "" "R" "" "B" "";
    turn_piece_test "Solved right p333" "p333" solved_cube "R"
      "counterclockwise" "" "Y" "R" "" "B" "";
    turn_piece_test "Solved top p113" "p113" solved_cube "U"
      "counterclockwise" "O" "" "" "B" "W" "";
    turn_piece_test "Solved top p213" "p213" solved_cube "U"
      "counterclockwise" "O" "" "" "" "W" "";
    turn_piece_test "Solved top p313" "p313" solved_cube "U"
      "counterclockwise" "O" "" "G" "" "W" "";
    turn_piece_test "Solved top p123" "p123" solved_cube "U"
      "counterclockwise" "" "" "" "B" "W" "";
    turn_piece_test "Solved top p323" "p323" solved_cube "U"
      "counterclockwise" "" "" "G" "" "W" "";
    turn_piece_test "Solved top p133" "p133" solved_cube "U"
      "counterclockwise" "" "R" "" "B" "W" "";
    turn_piece_test "Solved top p233" "p233" solved_cube "U"
      "counterclockwise" "" "R" "" "" "W" "";
    turn_piece_test "Solved top p333" "p333" solved_cube "U"
      "counterclockwise" "" "R" "G" "" "W" "";
    turn_piece_test "Solved back p331" "p331" solved_cube "B"
      "counterclockwise" "" "B" "W" "" "" "R";
    turn_piece_test "Solved back p231" "p231" solved_cube "B"
      "counterclockwise" "" "B" "" "" "" "R";
    turn_piece_test "Solved back p131" "p131" solved_cube "B"
      "counterclockwise" "" "B" "" "Y" "" "R";
    turn_piece_test "Solved back p332" "p332" solved_cube "B"
      "counterclockwise" "" "B" "W" "" "" "";
    turn_piece_test "Solved back p132" "p132" solved_cube "B"
      "counterclockwise" "" "B" "" "Y" "" "";
    turn_piece_test "Solved back p333" "p333" solved_cube "B"
      "counterclockwise" "" "B" "W" "" "O" "";
    turn_piece_test "Solved back p233" "p233" solved_cube "B"
      "counterclockwise" "" "B" "" "" "O" "";
    turn_piece_test "Solved back p133" "p133" solved_cube "B"
      "counterclockwise" "" "B" "" "Y" "O" "";
    (*srambled tests: *)
    turn_piece_test "Scrambled right p311" "p311" scrambled_cube "R"
      "clockwise" "G" "" "Y" "" "" "O";
    turn_piece_test "Scrambled right p321" "p321" scrambled_cube "R"
      "clockwise" "" "" "W" "" "" "R";
    turn_piece_test "Scrambled right p331" "p331" scrambled_cube "R"
      "clockwise" "" "B" "O" "" "" "W";
    turn_piece_test "Scrambled right p312" "p312" scrambled_cube "R"
      "clockwise" "R" "" "B" "" "" "";
    turn_piece_test "Scrambled right p332" "p332" scrambled_cube "R"
      "clockwise" "" "Y" "O" "" "" "";
    turn_piece_test "Scrambled right p313" "p313" scrambled_cube "R"
      "clockwise" "W" "" "O" "" "G" "";
    turn_piece_test "Scrambled right p323" "p323" scrambled_cube "R"
      "clockwise" "" "" "O" "" "W" "";
    turn_piece_test "Scrambled right p333" "p333" scrambled_cube "R"
      "clockwise" "" "O" "Y" "" "B" "";
    turn_piece_test "Scrambled top p113" "p113" scrambled_cube "U"
      "clockwise" "Y" "" "" "B" "O" "";
    turn_piece_test "Scrambled top p213" "p213" scrambled_cube "U"
      "clockwise" "O" "" "" "" "Y" "";
    turn_piece_test "Scrambled top p313" "p313" scrambled_cube "U"
      "clockwise" "O" "" "W" "" "B" "";
    turn_piece_test "Scrambled top p123" "p123" scrambled_cube "U"
      "clockwise" "" "" "" "O" "B" "";
    turn_piece_test "Scrambled top p323" "p323" scrambled_cube "U"
      "clockwise" "" "" "B" "" "Y" "";
    turn_piece_test "Scrambled top p133" "p133" scrambled_cube "U"
      "clockwise" "" "B" "" "R" "Y" "";
    turn_piece_test "Scrambled top p233" "p233" scrambled_cube "U"
      "clockwise" "" "Y" "" "" "R" "";
    turn_piece_test "Scrambled top p333" "p333" scrambled_cube "U"
      "clockwise" "" "R" "Y" "" "G" "";
    turn_piece_test "Scrambled back p331" "p331" scrambled_cube "B"
      "clockwise" "" "W" "R" "" "" "B";
    turn_piece_test "Scrambled back p231" "p231" scrambled_cube "B"
      "clockwise" "" "R" "" "" "" "G";
    turn_piece_test "Scrambled back p131" "p131" scrambled_cube "B"
      "clockwise" "" "Y" "" "G" "" "R";
    turn_piece_test "Scrambled back p332" "p332" scrambled_cube "B"
      "clockwise" "" "G" "Y" "" "" "";
    turn_piece_test "Scrambled back p132" "p132" scrambled_cube "B"
      "clockwise" "" "B" "" "Y" "" "";
    turn_piece_test "Scrambled back p333" "p333" scrambled_cube "B"
      "clockwise" "" "O" "G" "" "Y" "";
    turn_piece_test "Scrambled back p233" "p233" scrambled_cube "B"
      "clockwise" "" "R" "" "" "W" "";
    turn_piece_test "Scrambled back p133" "p133" scrambled_cube "B"
      "clockwise" "" "W" "" "B" "O" "";
    (*counterclockwise tests: *)
    turn_piece_test "Scrambled front p111" "p111" scrambled_cube "F"
      "counterclockwise" "R" "" "" "Y" "" "B";
    turn_piece_test "Scrambled front p211" "p211" scrambled_cube "F"
      "counterclockwise" "B" "" "" "" "" "W";
    turn_piece_test "Scrambled front p311" "p311" scrambled_cube "F"
      "counterclockwise" "G" "" "W" "" "" "R";
    turn_piece_test "Scrambled front p112" "p112" scrambled_cube "F"
      "counterclockwise" "O" "" "" "B" "" "";
    turn_piece_test "Scrambled front p312" "p312" scrambled_cube "F"
      "counterclockwise" "W" "" "G" "" "" "";
    turn_piece_test "Scrambled front p113" "p113" scrambled_cube "F"
      "counterclockwise" "B" "" "" "O" "Y" "";
    turn_piece_test "Scrambled front p213" "p213" scrambled_cube "F"
      "counterclockwise" "W" "" "" "" "O" "";
    turn_piece_test "Scrambled front p313" "p313" scrambled_cube "F"
      "counterclockwise" "G" "" "W" "" "O" "";
    turn_piece_test "Scrambled left p131" "p131" scrambled_cube "L"
      "counterclockwise" "" "G" "" "R" "" "Y";
    turn_piece_test "Scrambled left p121" "p121" scrambled_cube "L"
      "counterclockwise" "" "" "" "G" "" "R";
    turn_piece_test "Scrambled left p111" "p111" scrambled_cube "L"
      "counterclockwise" "R" "" "" "B" "" "W";
    turn_piece_test "Scrambled left p132" "p132" scrambled_cube "L"
      "counterclockwise" "" "R" "" "Y" "" "";
    turn_piece_test "Scrambled left p112" "p112" scrambled_cube "L"
      "counterclockwise" "G" "" "" "O" "" "";
    turn_piece_test "Scrambled left p133" "p133" scrambled_cube "L"
      "counterclockwise" "" "Y" "" "B" "R" "";
    turn_piece_test "Scrambled left p123" "p123" scrambled_cube "L"
      "counterclockwise" "" "" "" "W" "B" "";
    turn_piece_test "Scrambled left p113" "p113" scrambled_cube "L"
      "counterclockwise" "W" "" "" "R" "G" "";
    turn_piece_test "Scrambled bottom p111" "p111" scrambled_cube "D"
      "counterclockwise" "O" "" "" "G" "" "W";
    turn_piece_test "Scrambled bottom p211" "p211" scrambled_cube "D"
      "counterclockwise" "B" "" "" "" "" "R";
    turn_piece_test "Scrambled bottom p311" "p311" scrambled_cube "D"
      "counterclockwise" "Y" "" "O" "" "" "G";
    turn_piece_test "Scrambled bottom p121" "p121" scrambled_cube "D"
      "counterclockwise" "" "" "" "W" "" "G";
    turn_piece_test "Scrambled bottom p321" "p321" scrambled_cube "D"
      "counterclockwise" "" "" "G" "" "" "Y";
    turn_piece_test "Scrambled bottom p131" "p131" scrambled_cube "D"
      "counterclockwise" "" "R" "" "G" "" "W";
    turn_piece_test "Scrambled bottom p231" "p231" scrambled_cube "D"
      "counterclockwise" "" "O" "" "" "" "G";
    turn_piece_test "Scrambled bottom p331" "p331" scrambled_cube "D"
      "counterclockwise" "" "B" "W" "" "" "R";
  ]

let slice_tests =
  [
    turn_piece_test "Solved mslice p211" "p211" solved_cube "M"
      "clockwise" "Y" "" "" "" "" "B";
    turn_piece_test "Solved mslice p212" "p212" solved_cube "M"
      "clockwise" "Y" "" "" "" "" "";
    turn_piece_test "Solved mslice p213" "p213" solved_cube "M"
      "clockwise" "Y" "" "" "" "G" "";
    turn_piece_test "Solved mslice p223" "p223" solved_cube "M"
      "clockwise" "" "" "" "" "G" "";
    turn_piece_test "Solved mslice p233" "p233" solved_cube "M"
      "clockwise" "" "W" "" "" "G" "";
    turn_piece_test "Solved mslice p232" "p232" solved_cube "M"
      "clockwise" "" "W" "" "" "" "";
    turn_piece_test "Solved mslice p231" "p231" solved_cube "M"
      "clockwise" "" "W" "" "" "" "B";
    turn_piece_test "Solved mslice p221" "p221" solved_cube "M"
      "clockwise" "" "" "" "" "" "B";
    turn_piece_test "Solved sslice p121" "p121" solved_cube "S"
      "clockwise" "" "" "" "Y" "" "R";
    turn_piece_test "Solved sslice p122" "p122" solved_cube "S"
      "clockwise" "" "" "" "Y" "" "";
    turn_piece_test "Solved sslice p123" "p123" solved_cube "S"
      "clockwise" "" "" "" "Y" "O" "";
    turn_piece_test "Solved sslice p223" "p223" solved_cube "S"
      "clockwise" "" "" "" "" "O" "";
    turn_piece_test "Solved sslice p323" "p323" solved_cube "S"
      "clockwise" "" "" "W" "" "O" "";
    turn_piece_test "Solved sslice p322" "p322" solved_cube "S"
      "clockwise" "" "" "W" "" "" "";
    turn_piece_test "Solved sslice p321" "p321" solved_cube "S"
      "clockwise" "" "" "W" "" "" "R";
    turn_piece_test "Solved sslice p221" "p221" solved_cube "S"
      "clockwise" "" "" "" "" "" "R";
    turn_piece_test "Solved eslice p112" "p112" solved_cube "E"
      "clockwise" "R" "" "" "G" "" "";
    turn_piece_test "Solved eslice p212" "p212" solved_cube "E"
      "clockwise" "R" "" "" "" "" "";
    turn_piece_test "Solved eslice p312" "p312" solved_cube "E"
      "clockwise" "R" "" "B" "" "" "";
    turn_piece_test "Solved eslice p322" "p322" solved_cube "E"
      "clockwise" "" "" "B" "" "" "";
    turn_piece_test "Solved eslice p332" "p332" solved_cube "E"
      "clockwise" "" "O" "B" "" "" "";
    turn_piece_test "Solved eslice p232" "p232" solved_cube "E"
      "clockwise" "" "O" "" "" "" "";
    turn_piece_test "Solved eslice p132" "p132" solved_cube "E"
      "clockwise" "" "O" "" "G" "" "";
    turn_piece_test "Solved eslice p122" "p122" solved_cube "E"
      "clockwise" "" "" "" "G" "" "";
  ]

let random_side sides =
  let num = Random.int 6 in
  match num with
  | 0 -> "F"
  | 1 -> "B"
  | 2 -> "L"
  | 3 -> "R"
  | 4 -> "U"
  | 5 -> "D"
  | _ -> "F"

let random_direction =
  let num = Random.int 1 in
  match num with
  | 0 -> "counterclockwise"
  | 1 -> "clockwise"
  | _ -> "counterclockwise"

let rec scramble cube counter : Cube.t =
  Random.self_init ();
  if counter = 50 then cube
  else
    let side = random_side counter in
    let dir = random_direction in
    let new_cube = Cube.turn dir side cube in
    scramble new_cube (counter + 1)

let solve_test name edge cube piece =
  name >:: fun _ ->
  assert_equal edge (find_piece cube piece) ~printer:print_piece

let test_cross =
  let scrambled_cube = scramble solved_cube 0 in
  let cross_cube = Solve.make_cross scrambled_cube in
  let white_layer = Solve.get_white_corners cross_cube in

  let second_layer = Solve.solve_second_layer white_layer in
  (*let top_cross = Solve.cross_on_top white_layer in*)
  Cube.print_all_faces white_layer;

  let wb_edge = make_piece "" "B" "" "" "W" "" in
  let wo_edge = make_piece "" "" "" "O" "W" "" in
  let wg_edge = make_piece "G" "" "" "" "W" "" in
  let wr_edge = make_piece "" "" "R" "" "W" "" in
  let wrb_corner = make_piece "" "B" "R" "" "W" "" in
  let wbo_corner = make_piece "" "B" "" "O" "W" "" in
  let wog_corner = make_piece "G" "" "" "O" "W" "" in
  let wgr_corner = make_piece "G" "" "R" "" "W" "" in
  let rg_edge = make_piece "G" "" "" "R" "" "" in
  let rb_edge = make_piece "" "B" "" "R" "" "" in
  let bo_edge = make_piece "" "B" "O" "" "" "" in
  let og_edge = make_piece "G" "" "O" "" "" "" in

  let yb_edge = make_piece "" "B" "" "" "Y" "" in
  let yo_edge = make_piece "" "" "O" "" "Y" "" in
  let yg_edge = make_piece "G" "" "" "" "Y" "" in
  let yr_edge = make_piece "" "" "" "R" "Y" "" in
  [
    solve_test "white blue edge" wb_edge cross_cube "p233";
    solve_test "white orange edge" wo_edge cross_cube "p123";
    solve_test "white green edge" wg_edge cross_cube "p213";
    solve_test "white red edge" wr_edge cross_cube "p323";
    solve_test "white red blue corner" wrb_corner white_layer "p333";
    solve_test "white blue orange corner" wbo_corner white_layer "p133";
    solve_test "white orange green corner" wog_corner white_layer "p113";
    solve_test "white green red corner" wgr_corner white_layer "p313";
    solve_test "red blue edge" rb_edge second_layer "p132";
    solve_test "red green edge" rg_edge second_layer "p112";
    solve_test "blue orange green edge" bo_edge second_layer "p332";
    solve_test "orange green edge" og_edge second_layer "p312";
    (*solve_test "yellow blue edge" yb_edge top_cross "p233"; solve_test
      "yellow orange edge" yo_edge top_cross "p323"; solve_test "yellow
      green edge" yg_edge top_cross "p213"; solve_test "yellow red edge"
      yr_edge top_cross "p123";*)
  ]

let top_cross_tests =
  let cube =
    Cube.print_apply_turns
      [
        "S2";
        "F2";
        "B2";
        "R";
        "U";
        "R'";
        "U";
        "R";
        "U2";
        "R'";
        "F";
        "R";
        "U";
        "R'";
        "U'";
        "R";
        "U";
        "R'";
        "U'";
        "F'";
        "U";
        "F";
        "R";
        "U";
        "R'";
        "U'";
        "F'";
      ]
      solved_cube
  in
  let cube1 = Solve.cross_on_top cube in
  let cube2 = Solve.top_edges_align cube1 in
  let yb_edge2 = make_piece "" "B" "" "" "Y" "" in
  let yo_edge2 = make_piece "" "" "O" "" "Y" "" in
  let yg_edge2 = make_piece "G" "" "" "" "Y" "" in
  let yr_edge2 = make_piece "" "" "" "R" "Y" "" in
  [
    solve_test "yellow blue edge" yb_edge2 cube2 "p233";
    solve_test "yellow orange edge" yo_edge2 cube2 "p323";
    solve_test "yellow green edge" yg_edge2 cube2 "p213";
    solve_test "yellow red edge" yr_edge2 cube2 "p123";
  ]

let two_layer_lst =
  [
    ("p111", Piece.make_piece "G" "" "" "R" "" "W");
    ("p112", Piece.make_piece "G" "" "" "R" "" "");
    ("p113", Piece.make_piece "R" "" "" "B" "Y" "");
    ("p121", Piece.make_piece "" "" "" "R" "" "W");
    ("p122", Piece.make_piece "" "" "" "R" "" "");
    ("p123", Piece.make_piece "" "" "" "R" "Y" "");
    ("p131", Piece.make_piece "" "B" "" "R" "" "W");
    ("p132", Piece.make_piece "" "B" "" "R" "" "");
    ("p133", Piece.make_piece "" "Y" "" "O" "B" "");
    ("p211", Piece.make_piece "G" "" "" "" "" "W");
    ("p212", Piece.make_piece "G" "" "" "" "" "");
    ("p213", Piece.make_piece "G" "" "" "" "Y" "");
    ("p221", Piece.make_piece "" "" "" "" "" "W");
    ("p223", Piece.make_piece "" "" "" "" "Y" "");
    ("p231", Piece.make_piece "" "B" "" "" "" "W");
    ("p232", Piece.make_piece "" "B" "" "" "" "");
    ("p233", Piece.make_piece "" "B" "" "" "Y" "");
    ("p311", Piece.make_piece "G" "" "O" "" "" "W");
    ("p312", Piece.make_piece "G" "" "O" "" "" "");
    ("p313", Piece.make_piece "G" "" "O" "" "Y" "");
    ("p321", Piece.make_piece "" "" "O" "" "" "W");
    ("p322", Piece.make_piece "" "" "O" "" "" "");
    ("p323", Piece.make_piece "" "" "O" "" "Y" "");
    ("p331", Piece.make_piece "" "B" "O" "" "" "W");
    ("p332", Piece.make_piece "" "B" "O" "" "" "");
    ("p333", Piece.make_piece "" "Y" "G" "" "R" "");
  ]

let two_layer_cube = make_cube two_layer_lst

let test_yellow_corners =
  let y_corner_cube = Solve.check_yellow_corners two_layer_cube in

  let gry_corner = make_piece "G" "" "" "R" "Y" "" in
  let goy_corner = make_piece "G" "" "O" "" "Y" "" in
  let boy_corner = make_piece "" "B" "O" "" "Y" "" in
  let bry_corner = make_piece "" "B" "" "R" "Y" "" in

  [
    solve_test "gry corner" gry_corner y_corner_cube "p113";
    solve_test "goy corner" goy_corner y_corner_cube "p313";
    solve_test "boy corner" boy_corner y_corner_cube "p333";
    solve_test "bry corner" bry_corner y_corner_cube "p133";
  ]

let test_suites =
  "all tests"
  >::: List.flatten
         [
           representation_tests;
           turn_tests;
           slice_tests;
           test_cross;
           top_cross_tests;
           test_yellow_corners;
         ]

let main () = run_test_tt_main test_suites

let () = main ()
