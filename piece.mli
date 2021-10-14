(** Representation of an individual piece in a Rubik's cube.

    This module represents the data that is stored in each piece. *)

(**********************************************************************)

(** The abstract type of values representing a piece in Rubik's cube.
    The colors are stored in the orientation of t, and are color options
    if there is no color in a specific orientation. *)

type colors

type color

type t
(** [make_piece f b r l u d] represents an instance of a piece with the colors 
    given in the appropriate positions. If  any argument is not a valid color 
    or ["none"], [None] will be put in that position
    Requires: The arguments represent a piece that actually exists on a real 
    Rubik's cube. For example, a piece should not have two different sides of 
    the same color *)
val make_piece : string -> string -> string -> string -> string -> string -> t

(** [get_right p] gets the Color Option on the right side of [p]*)
val get_right : t -> color

(** [get_left p] gets the Color Option on the left side of [p]*)
val get_left : t -> color

(** [get_front p] gets the Color Option on the front side of [p]*)
val get_front : t -> color

(** [get_back p] gets the Color Option on the back side of [p]*)
val get_back : t -> color

(** [get_up p] gets the Color Option on the top side of [p]*)
val get_up : t -> color

(** [get_down p] gets the Color Option on the bottom side of [p]*)
val get_down : t -> color

(** [set_right p c] sets the Color Option [c] on the right side of [p]*)
val set_right : t -> color -> unit

(** [set_left p c] sets the Color Option [c] on the left side of [p]*)
val set_left : t -> color -> unit

(** [set_front p c] sets the Color Option [c] on the front side of [p]*)
val set_front : t -> color -> unit

(** [set_back p c] sets the Color Option [c] on the back side of [p]*)
val set_back : t -> color -> unit

(** [set_up p c] sets the Color Option [c] on the top side of [p]*)
val set_up : t -> color -> unit

(** [set_down p c] sets the Color Option [c] on the bottom side of [p]*)
val set_down : t -> color -> unit

(** [string_of_color c] represents the color [c] as a string.*)
val string_of_color: color -> string