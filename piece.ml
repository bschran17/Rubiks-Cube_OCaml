type colors =
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | White

type color =
  | None
  | Some of colors

type t = {
  mutable front : color;
  mutable back : color;
  mutable right : color;
  mutable left : color;
  mutable up : color;
  mutable down : color;
}

let match_color = function
  | "R" -> Some Red
  | "O" -> Some Orange
  | "Y" -> Some Yellow
  | "G" -> Some Green
  | "B" -> Some Blue
  | "W" -> Some White
  | "N" -> None
  | _ -> None

let make_piece f b r l u d =
  {
    front = match_color f;
    back = match_color b;
    right = match_color r;
    left = match_color l;
    up = match_color u;
    down = match_color d;
  }

let get_right piece = piece.right

let get_left piece = piece.left

let get_front piece = piece.front

let get_back piece = piece.back

let get_up piece = piece.up

let get_down piece = piece.down

let set_right piece clr = piece.right <- clr

let set_left piece clr = piece.left <- clr

let set_up piece clr = piece.up <- clr

let set_down piece clr = piece.down <- clr

let set_front piece clr = piece.front <- clr

let set_back piece clr = piece.back <- clr

let determine_color clrs =
  match clrs with
  | Red -> "R"
  | Orange -> "O"
  | Yellow -> "Y"
  | Green -> "G"
  | Blue -> "B"
  | White -> "W"

let string_of_color clr =
  match clr with Some h -> determine_color h | None -> "N"
