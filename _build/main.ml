open Cube
open Piece
open Random
open Solve
open Test

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

let num_to_direct num =
  if num = "1" then "clockwise"
  else if num = "2" then "counterclockwise"
  else ""

let rec turn_interface cube =
  ANSITerminal.print_string
    [ ANSITerminal.yellow; ANSITerminal.Bold; ANSITerminal.Underlined ]
    "\n\
     Enter a face to turn. (you can also enter mslice, eslice, or \
     sslice for slice moves)\n";
  let str = read_line () in
  ANSITerminal.print_string
    [ ANSITerminal.yellow; ANSITerminal.Bold; ANSITerminal.Underlined ]
    "\nEnter 1 for Clockwise and Enter 2 for Counterclockwise.\n";
  let dir = read_line () in
  let direction = num_to_direct dir in
  if direction = "" then turn_interface cube
  else
    match str with
    | "top" ->
        let new_cube = Cube.turn direction "U" cube in
        Cube.print_unfolded new_cube;
        new_cube
    | "bottom" ->
        let new_cube = Cube.turn direction "D" cube in
        Cube.print_unfolded new_cube;
        new_cube
    | "left" ->
        let new_cube = Cube.turn direction "L" cube in
        Cube.print_unfolded new_cube;
        new_cube
    | "right" ->
        let new_cube = Cube.turn direction "R" cube in
        Cube.print_unfolded new_cube;
        new_cube
    | "front" ->
        let new_cube = Cube.turn direction "F" cube in
        Cube.print_unfolded new_cube;
        new_cube
    | "back" ->
        let new_cube = Cube.turn direction "B" cube in
        Cube.print_unfolded new_cube;
        new_cube
    | "mslice" ->
        let new_cube = Cube.turn direction "M" cube in
        Cube.print_unfolded new_cube;
        new_cube
    | "eslice" ->
        let new_cube = Cube.turn direction "E" cube in
        Cube.print_unfolded new_cube;
        new_cube
    | "sslice" ->
        let new_cube = Cube.turn direction "S" cube in
        Cube.print_unfolded new_cube;
        new_cube
    | _ ->
        ANSITerminal.print_string
          [ ANSITerminal.Bold; ANSITerminal.red ]
          "You entered an Invalid Input, try again.\n";
        turn_interface cube

let rec rotation_interface cube =
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Bold; ANSITerminal.Underlined ]
    "\nEnter cube rotation (x, y, z).\n";
  let str = read_line () in
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Bold; ANSITerminal.Underlined ]
    "\nEnter 1 for Clockwise and Enter 2 for Counterclockwise.\n";
  let dir = read_line () in
  let direction = num_to_direct dir in
  if direction = "" then turn_interface cube
  else
    match str with
    | "x" ->
        let new_cube = Cube.change_orientation "X" direction cube in
        Cube.print_unfolded new_cube;
        new_cube
    | "y" ->
        let new_cube = Cube.change_orientation "Y" direction cube in
        Cube.print_unfolded new_cube;
        new_cube
    | "z" ->
        let new_cube = Cube.change_orientation "Z" direction cube in
        Cube.print_unfolded new_cube;
        new_cube
    | _ ->
        ANSITerminal.print_string
          [ ANSITerminal.Bold; ANSITerminal.red ]
          "You entered an Invalid Input, try 'x'.\n";
        rotation_interface cube

let rec get_input cube =
  while 1 > 0 do
    ANSITerminal.print_string
      [ ANSITerminal.Bold; ANSITerminal.white ]
      "Enter command here:";
    let str = read_line () in
    match str with
    | "unfolded" -> Cube.print_unfolded cube
    | "all_faces" -> Cube.print_all_faces cube
    | "front" -> Cube.print_face cube "F"
    | "back" -> Cube.print_face cube "B"
    | "left" -> Cube.print_face cube "L"
    | "right" -> Cube.print_face cube "R"
    | "top" -> Cube.print_face cube "U"
    | "bottom" -> Cube.print_face cube "D"
    | "turn" ->
        let new_cube = turn_interface cube in
        get_input new_cube
    | "scramble" ->
        let new_cube = scramble cube 0 in
        get_input new_cube
    | "cube_rotation" ->
        let new_cube = rotation_interface cube in
        get_input new_cube
    | "solve" -> get_input (Solve.solve cube)
    | "quit" ->
        ANSITerminal.print_string
          [ ANSITerminal.Bold; ANSITerminal.cyan ]
          "Thanks for playing!!\n";
        exit 0
    | _ ->
        ANSITerminal.print_string
          [ ANSITerminal.Bold; ANSITerminal.red ]
          "You entered an Invalid Input, try 'cube_unfolded' or 'front'.\n"
  done

let main () =
  let solved_lst = Cube.make_solved_lst in
  let cube = make_cube solved_lst in
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold; ANSITerminal.Underlined ]
    "\nWelcome to 3110 Rubik's Cube\n";
  ANSITerminal.print_string
    [ ANSITerminal.blue; ANSITerminal.Bold ]
    "If you'd like to see your cube unfolded at any time, enter \
     'unfolded'\n";
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Bold ]
    "If you'd like to see all individual faces of a cube, enter \
     'all_faces'.\n";
  ANSITerminal.print_string
    [ ANSITerminal.yellow; ANSITerminal.Bold ]
    "If you'd like to see a single face of a cube, enter a face to \
     display\n";
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "Valid Faces: 'right', 'left', 'top', 'bottom', 'front', 'back'\n";
  ANSITerminal.print_string
    [ ANSITerminal.blue; ANSITerminal.Bold ]
    "If you'd like to turn the cube, enter 'turn'\n";
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Bold ]
    "Wanna scramble your cube? Enter 'scramble'\n";
  ANSITerminal.print_string
    [ ANSITerminal.yellow; ANSITerminal.Bold ]
    "Can you do a Cube Rotation? Absolutely, try 'cube_rotation'\n";
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "To solve the cube automatically using the Beginner's method, type \
     'solve'\n";
  ANSITerminal.print_string
    [ ANSITerminal.blue; ANSITerminal.Bold ]
    "To quit playing, enter 'quit'.\n";
  get_input cube

let () = main ()
