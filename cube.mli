(** Representation of a Rubik's cube.

    This module represents the data that is needed to represent a
    Rubik's cube as well as its functionalities *)

(**********************************************************************)

(** The abstract type of values representing a Rubik's cube. For
    simplicity, p111 represents the bottom left corner of the cube. The
    x-axis goes out to the right, y-axis goes away toward you, and
    z-axis goes straight up. p222 is not included since it is the center
    of the cube and has no color or orientation*)
type t

(**Raised when the piece is invalid*)
exception InvalidPiece

(**Raised when an invalid orientation is entered*)
exception InvalidOrientation

(**Raised when the face is invalid*)
exception InvalidFace

(** [make_cube lst] initializes a cube of type t with [lst]*)
val make_cube : (string * Piece.t) list -> t

(** [turn direction face cube] rotates only the specified face of the
    cube in [direction], which can be ["clockwise"] or
    ["counterclockwise"], when looking directly at that face. For this
    function, each face is represented by the capitalized version of its
    first letter (example: right -> ["R"]) If [direction] is not input
    as ["clockwise"], it is treated as ["counterclockwise"].
    Counterclockwise rotations are implemented by doing 3 clockwise
    rotations in a row. This function also performs an m-slice, e-slice,
    and s-slice if the face is passed in as ["M"], ["E"], and ["S"],
    respectivley. These funcitons can have clockwise and
    counterclockwise turns, which are oriented from the Right, Up, and
    Front faces, respectively (but the middle rows/columns of pieces
    will change instead). Raises [InvalidOrientation] if face is not a
    valid input*)
val turn : string -> string -> t -> t

(**[find_piece cube piece] is the [Piece.t] that is associated with the
   string [piece] on [cube]. Requires: [piece] is a valid piece name. It
   must be of the form ["pxxx"], where each [x] is 1, 2, or 3. The only
   invalid combination of numbers is 222. *)
val find_piece : t -> string -> Piece.t

(** [get_color c p o] gets the color out of the piece [p] in cube [c]
    with orientation [o]. Requires: [p] is a valid piece in [c], and
    that [o] is a valid orientation type of [p]. *)
val get_color : Piece.t -> string -> string

(** [print_row c f r] prints out the row r of face f in cube c.
    Requires: [r] is an int from 1-3, where 1 represents the bottom row
    when face [f] is facing you. In addition, [f] is one of the
    following strings: ["front"; "back"; "left"; "right"; "up"; "down"].*)
val print_row : t -> string -> int -> unit

(** [print_face c f ] prints out face f in cube c. Requires: [f] is one
    of the following strings:
    ["front"; "back"; "left"; "right"; "up"; "down"].*)
val print_face : t -> string -> unit

(** [print_all_faces c] prints out all the faces in c in a list-like
    manner. *)
val print_all_faces : t -> unit

(** [print_unfolded c] prints out all the faces in c in a 2D
    representation. *)
val print_unfolded : t -> unit

(** [make_solved_lst] returns the list of pieces needed to create a
    solved cube *)
val make_solved_lst : (string * Piece.t) list

(** [make_scrambled_lst] returns the list of pieces needed to create a
    scrambled cube *)
val make_scram_lst : (string * Piece.t) list

(** [get_p111 cube] returns the piece at position p111*)
val get_p111 : t -> Piece.t

(** [get_p112 cube] returns the piece at position p112*)
val get_p112 : t -> Piece.t

(** [get_p113 cube] returns the piece at position p113*)
val get_p113 : t -> Piece.t

(** [get_p211 cube] returns the piece at position p211*)
val get_p211 : t -> Piece.t

(** [get_p212 cube] returns the piece at position p212*)
val get_p212 : t -> Piece.t

(** [get_p213 cube] returns the piece at position p213*)
val get_p213 : t -> Piece.t

(** [get_p311 cube] returns the piece at position p311*)
val get_p311 : t -> Piece.t

(** [get_p312 cube] returns the piece at position p312*)
val get_p312 : t -> Piece.t

(** [get_p313 cube] returns the piece at position p313*)
val get_p313 : t -> Piece.t

(** [get_p121 cube] returns the piece at position p121*)
val get_p121 : t -> Piece.t

(** [get_p122 cube] returns the piece at position p122*)
val get_p122 : t -> Piece.t

(** [get_p123 cube] returns the piece at position p123*)
val get_p123 : t -> Piece.t

(** [get_p221 cube] returns the piece at position p221*)
val get_p221 : t -> Piece.t

(** [get_p223 cube] returns the piece at position p223*)
val get_p223 : t -> Piece.t

(** [get_p321 cube] returns the piece at position p321*)
val get_p321 : t -> Piece.t

(** [get_p322 cube] returns the piece at position p322*)
val get_p322 : t -> Piece.t

(** [get_p323 cube] returns the piece at position p323*)
val get_p323 : t -> Piece.t

(** [get_p131 cube] returns the piece at position p131*)
val get_p131 : t -> Piece.t

(** [get_p132 cube] returns the piece at position p111*)
val get_p132 : t -> Piece.t

(** [get_p133 cube] returns the piece at position p133*)
val get_p133 : t -> Piece.t

(** [get_p231 cube] returns the piece at position p231*)
val get_p231 : t -> Piece.t

(** [get_p232 cube] returns the piece at position p232*)
val get_p232 : t -> Piece.t

(** [get_p233 cube] returns the piece at position p233*)
val get_p233 : t -> Piece.t

(** [get_p331 cube] returns the piece at position p331*)
val get_p331 : t -> Piece.t

(** [get_p323 cube] returns the piece at position p323*)
val get_p332 : t -> Piece.t

(** [get_p333 cube] returns the piece at position p333*)
val get_p333 : t -> Piece.t

(**[change_orientation ax dir c] performs a cube rotation of [c] on the
   axis ax, in the directions dir.*)
val change_orientation : string -> string -> t -> t

(**[print_apply_turns lst cube] is the cube resulting from applying the
   list of moves, [lst] to [cube]. In between each move, the resulting
   cube is printed to the console (using [print_unfolded]). If no move
   is executed, the cube is not printed*)
val print_apply_turns : string list -> t -> t
