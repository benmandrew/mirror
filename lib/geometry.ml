type pos = { x : float; y : float }

let add_pos { x = x0; y = y0 } { x = x1; y = y1 } =
  { x = x0 +. x1; y = y0 +. y1 }

type circle = { p : pos; r : float }
type polygon = { p : pos; r : float; rot_angle : float; n_segments : int }

let twopi = 2. *. Float.pi

(** [angles ~closed ~offset n] generates a list of equally spaced angles
    (in radians), starting from [offset]. If [closed = true] then we
    repeat the first angle at the end of the list to close the loop,
    and thus have [n+1] elements. Otherwise, we have [n] elements. *)
let angles ?(offset = 0.) n =
  List.init n (fun i -> (float_of_int i *. twopi /. float_of_int n) +. offset)

let fractal_c (c : circle) ~n ~d ~r =
  let angles = angles n in
  let f a =
    let offset = { x = c.r *. d *. cos a; y = c.r *. d *. sin a } in
    { p = add_pos c.p offset; r = r *. c.r }
  in
  List.map f angles

(** Generate [n] new polygons arranged around [p].
    - [d] is the relative distance offset of each polygon's centre
    - [r] is the relative new radius of each polygon
      as a proportion of [p.r] *)
let fractal_p (p : polygon) ~n ~d ~r =
  let angles = angles n in
  let f a =
    let offset = { x = p.r *. d *. cos a; y = p.r *. d *. sin a } in
    {
      p = add_pos p.p offset;
      r = r *. p.r;
      n_segments = p.n_segments;
      rot_angle = p.rot_angle +. a +. (-.twopi /. 4.);
    }
  in
  List.map f angles

(** Recursively apply the fractal operation, with each element of
    [ns] being the number of newly generated polygons per polygon
    of the previous iteration. *)
let fractal_p_repeat (p : polygon) ~ns ~d ~r =
  List.fold_left
    (fun ps n ->
      let pss = List.map (fun p -> fractal_p p ~n ~d ~r) ps in
      List.concat pss)
    [ p ] ns
