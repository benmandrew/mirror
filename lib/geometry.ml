type pos = { x : float; y : float }

let add_pos { x = x0; y = y0 } { x = x1; y = y1 } =
  { x = x0 +. x1; y = y0 +. y1 }

type circle = { p : pos; r : float }
type polygon = { p : pos; r : float; rot_angle : float; n_segments : int }
type shape = C of circle | P of polygon

let cascade cs =
  List.fold_left (fun acc c -> if Int.equal acc 0 then c else acc) 0 cs

let float_compare f0 f1 =
  if Float.abs (f0 -. f1) < 0.0001 then 0 else Float.compare f0 f1

let pos_compare p0 p1 =
  cascade [ float_compare p0.x p1.x; float_compare p0.y p1.y ]

let circle_compare (c0 : circle) (c1 : circle) =
  cascade [ pos_compare c0.p c1.p; float_compare c0.r c1.r ]

(** Polygons may be rotated by different angles but still
    represent the same polygon, for example every square
    has 4 different possible angles. This function rotates
    the angle back around until it is at its lowest positive
    angle, so that they can be compared. *)
let min_angle a n =
  let rec make_positive a =
    if Float.compare a 0. >= 0 then a else make_positive (a +. (2. *. Float.pi))
  in
  let delta = 2. *. Float.pi /. float_of_int n in
  let rec get_min a =
    if Float.compare a delta <= 0 then a else get_min (a -. delta)
  in
  get_min @@ make_positive a

let polygon_compare p0 p1 =
  cascade
    [
      Int.compare p0.n_segments p1.n_segments;
      pos_compare p0.p p1.p;
      float_compare p0.r p1.r;
      float_compare
        (min_angle p0.rot_angle p0.n_segments)
        (min_angle p1.rot_angle p1.n_segments);
    ]

let shape_compare s0 s1 =
  match (s0, s1) with
  | C c0, C c1 -> circle_compare c0 c1
  | P p0, P p1 -> polygon_compare p0 p1
  | C _, P _ -> 1
  | P _, C _ -> 0

module Set = Set.Make (struct
  type t = shape

  let compare = shape_compare
end)

let get_r = function C c -> c.r | P p -> p.r
let get_rot_angle = function C _ -> 0. | P p -> p.rot_angle
let twopi = 2. *. Float.pi

(** [angles ~closed ~offset n] generates a list of [n] equally spaced
    angles (in radians), starting from angle [offset] *)
let angles ?(offset = 0.) n =
  List.init n (fun i -> (float_of_int i *. twopi /. float_of_int n) +. offset)
