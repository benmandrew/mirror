type pos = { x : float; y : float }

let add_pos { x = x0; y = y0 } { x = x1; y = y1 } =
  { x = x0 +. x1; y = y0 +. y1 }

type circle = { p : pos; r : float }
type polygon = { p : pos; r : float; rot_angle : float; n_segments : int }
type shape = C of circle | P of polygon

let get_r = function C c -> c.r | P p -> p.r
let get_rot_angle = function C _ -> 0. | P p -> p.rot_angle
let twopi = 2. *. Float.pi

(** [angles ~closed ~offset n] generates a list of [n] equally spaced
    angles (in radians), starting from angle [offset] *)
let angles ?(offset = 0.) n =
  List.init n (fun i -> (float_of_int i *. twopi /. float_of_int n) +. offset)
