open Conf
open Geometry

let circle cf ?(lw = 2.0) (c : circle) =
  Cairo.set_line_width cf.cr lw;
  Cairo.set_source_rgb cf.cr 1. 1. 1.;
  Cairo.arc cf.cr (c.p.x +. cf.hwf) (c.p.y +. cf.hwf) ~r:c.r ~a1:0. ~a2:twopi;
  Cairo.stroke cf.cr

let polygon cf ?(lw = 2.0) (p : polygon) =
  let angles = angles ~offset:p.rot_angle p.n_segments in
  Cairo.set_line_width cf.cr lw;
  Cairo.set_source_rgb cf.cr 1. 1. 1.;
  let offsets =
    List.map (fun a -> { x = p.r *. cos a; y = p.r *. sin a }) angles
  in
  let f offset =
    let pos = add_pos p.p offset in
    Cairo.line_to cf.cr (pos.x +. cf.hwf) (pos.y +. cf.hwf)
  in
  List.iter f offsets;
  Cairo.Path.close cf.cr;
  Cairo.stroke cf.cr

let v cf ?(lw = 2.0) = function
  | C c -> circle cf ~lw c
  | P p -> polygon cf ~lw p
