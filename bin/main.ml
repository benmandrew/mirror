open Mirror
open Geometry

let draw cf =
  let scale = 2.75 in
  let p0 =
    P
      {
        p = { x = 0.; y = 0. };
        r = 150. *. scale;
        n_segments = 5;
        rot_angle = -.Float.pi /. 2.;
      }
  in
  let p1 =
    P
      {
        p = { x = 0.; y = 0. };
        r = 150. *. scale;
        n_segments = 10;
        rot_angle = -.Float.pi /. 2.;
      }
  in
  let p2 =
    P
      {
        p = { x = 0.; y = 0. };
        r = 60. *. scale;
        n_segments = 7;
        rot_angle = -.Float.pi /. 2.;
      }
  in
  let p3 =
    P
      {
        p = { x = 0.; y = 0. };
        r = 50. *. scale;
        n_segments = 5;
        rot_angle = -.Float.pi /. 4.;
      }
  in
  let targets =
    [ (p1, 10, -.twopi /. 4., true); (p2, 20, 0., true); (p3, 0, 0., true) ]
  in
  let ps = fractal_repeat p0 targets ~d:2. ~r:1. in
  List.iter (Draw.v cf ~lw:2.0) ps

let () =
  let width = 1000 in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:width in
  let cf = Conf.init surface width in
  Cairo.set_source_rgb cf.cr 0.0 0.0 0.0;
  Cairo.paint cf.cr;
  draw cf;
  Cairo.PNG.write surface "output.png"
