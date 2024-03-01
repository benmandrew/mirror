open Geometry

type target = { s : Geometry.shape; n : int; r_offset : float; render : bool }
type tree = Leaf of target | Branch of target * tree list

(** Generate [n] new shapes arranged around [s_origin], templated on [s_target].
    - [d] is the relative distance offset of each polygon's
      centre as a proportion of [s_origin.r]
    - [r] is the relative new radius of each polygon
      as a proportion of [s_origin.r]
    - The shapes are arranged starting at angle [offset] *)
let v s_origin s_target ~n ~d ~r ~rot =
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
let repeat s_origin targets ~d ~r =
  List.fold_left
    (fun ss (s_target, n, rot, include_prev) ->
      let ss_new =
        List.concat
        @@ List.map (fun s_prev -> v s_prev s_target ~n ~d ~r ~rot) ss
      in
      if include_prev then ss_new @ ss else ss_new)
    [ s_origin ] targets
