open Geometry

type target = { s : shape; render : bool }

type reflect = {
  target : target;
  n : int;
  rot_offset : float;
  d : float;
  children : reflect list;
}

type tree_root = target * reflect list

let generate_shape target new_pos rot_angle a =
  match target.s with
  | C c -> C { p = new_pos; r = c.r }
  | P p ->
      P
        {
          p = new_pos;
          r = p.r;
          n_segments = p.n_segments;
          rot_angle = rot_angle +. a +. (-.twopi /. 4.);
        }

let rec reflect pos r { target; n; rot_offset; d; children } =
  let rot_angle = get_rot_angle target.s in
  let f a =
    let offset = { x = r *. d *. cos a; y = r *. d *. sin a } in
    let new_pos = add_pos pos offset in
    let this_shape =
      if target.render then [ generate_shape target new_pos rot_angle a ]
      else []
    in
    let this_r = get_r target.s in
    let child_shapes = List.map (reflect new_pos this_r) children in
    this_shape @ List.concat child_shapes
  in
  angles ~offset:rot_offset n |> List.map f |> List.concat

let v root children =
  let center = { x = 0.; y = 0. } in
  let rot_angle = get_rot_angle root.s in
  let initial =
    if root.render then Set.singleton @@ generate_shape root center rot_angle 0.
    else Set.empty
  in
  let r = get_r root.s in
  let child_shapes = List.concat @@ List.map (reflect center r) children in
  Printf.printf "%d\n" @@ List.length child_shapes;
  Set.union initial @@ Set.of_list child_shapes
