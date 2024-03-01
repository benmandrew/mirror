type pos = { x : float; y : float }

let add_pos { x = x0; y = y0 } { x = x1; y = y1 } =
  { x = x0 +. x1; y = y0 +. y1 }

type circle = { p : pos; r : float }
type polygon = { p : pos; r : float; rot_angle : float; n_segments : int }
type shape = C of circle | P of polygon

let get_pos = function C c -> c.p | P p -> p.p
let get_r = function C c -> c.r | P p -> p.r
let get_rot_angle = function C _ -> 0. | P p -> p.rot_angle
let twopi = 2. *. Float.pi

(** [angles ~closed ~offset n] generates a list of [n] equally spaced
    angles (in radians), starting from angle [offset] *)
let angles ?(offset = 0.) n =
  List.init n (fun i -> (float_of_int i *. twopi /. float_of_int n) +. offset)

let fractal_c (c : circle) ~n ~d ~r =
  let angles = angles n in
  let f a =
    let offset = { x = c.r *. d *. cos a; y = c.r *. d *. sin a } in
    C { p = add_pos c.p offset; r = r *. c.r }
  in
  List.map f angles

let fractal_p (p : polygon) ~n ~d ~r =
  let angles = angles n in
  let f a =
    let offset = { x = p.r *. d *. cos a; y = p.r *. d *. sin a } in
    P
      {
        p = add_pos p.p offset;
        r = r *. p.r;
        n_segments = p.n_segments;
        rot_angle = p.rot_angle +. a +. (-.twopi /. 4.);
      }
  in
  List.map f angles

(** Generate [n] new shapes arranged around [s_origin], templated on [s_target].
    - [d] is the relative distance offset of each polygon's
      centre as a proportion of [s_origin.r]
    - [r] is the relative new radius of each polygon
      as a proportion of [s_origin.r]
    - The shapes are arranged starting at angle [offset] *)
let fractal s_origin s_target ~n ~d ~r ~rot =
  let offset a =
    let r = get_r s_origin in
    { x = r *. d *. cos a; y = r *. d *. sin a }
  in
  let f a =
    let pos = get_pos s_origin in
    match s_target with
    | C c -> C { p = add_pos pos @@ offset a; r = r *. c.r }
    | P p ->
        P
          {
            p = add_pos pos @@ offset a;
            r = r *. p.r;
            n_segments = p.n_segments;
            rot_angle = get_rot_angle s_origin +. a +. (-.twopi /. 4.);
          }
  in
  List.map f @@ angles ~offset:rot n

(** Recursively apply the fractal operation, with each element of
    [ns] being the number of newly generated shapes per shape
    of the previous iteration. *)
let fractal_repeat s_origin targets ~d ~r =
  List.fold_left
    (fun ss (s_target, n, rot, include_prev) ->
      let ss_new =
        List.concat
        @@ List.map (fun s_prev -> fractal s_prev s_target ~n ~d ~r ~rot) ss
      in
      if include_prev then ss_new @ ss else ss_new)
    [ s_origin ] targets
