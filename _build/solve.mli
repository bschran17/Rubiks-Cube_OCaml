(**Representation of the algorithms that will solve a Rubik's Cube.t

   This module contains the functions that are used to solve a Rubik's
   cube using the Beginner's Method*)

(**********************************************************************)

(** The abstract type representing the sequence of moves that are taken
    to solve the cube*)
type t

(**[solve cube] is a solved Rubik's cube after executing the necessary
   turns on [cube] to move it to a solved state*)
val solve : Cube.t -> Cube.t

(**[make_cross cube] is [cube] but with turns applied so that the white
   cross is present (the parts of the cross will also be correctly
   aligned with the faces on the sides of the cube)*)
val make_cross : Cube.t -> Cube.t

(**[get_white_corners cube] is [cube] but with turns applied so that the
   white corners are put in place and oriented correctly. The first
   layer of the cube will be solved when this function completes. This
   function assumes that [cube] already has the white cross solved.*)
val get_white_corners : Cube.t -> Cube.t

(**[solve_second_layer cube] is [cube] but with turns applied so that
   the second layer is solved. This function assumes that [cube] already
   has the first layer solved*)
val solve_second_layer : Cube.t -> Cube.t

(**[cross_on_top cube] is [cube] but with turns applied so that the
   yellow cross on the third layer is present (the parts of the cross
   will not necessarily be correctly aligned with the faces on the sides
   of the cube) This function assumes that [cube] already has the first
   two layers solved. *)
val cross_on_top : Cube.t -> Cube.t

(** [top_edges_align cube] is [cube] but with turns applied so that the
    third layer has a yellow cross with the edges aligned with the signs
    of the cube. This function assumes that [cube] already has the first
    two layers solved and the yellow cross appears in the third layer. *)
val top_edges_align : Cube.t -> Cube.t

(**[check_yellow_corners cube] is [cube] but completely solved. It
   places the yellow corners in the correct place and then orients them
   correctly. This function assumes that [cube] already has the first
   two layers solved and the yellow cross appears in the third layer
   with its edges correctly alligned with the sides of the cube*)
val check_yellow_corners : Cube.t -> Cube.t
