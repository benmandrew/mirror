open Mirror
open Geometry

let draw cf =
  let scale = 24. in
  let t0 =
    Fractal.
      {
        s =
          P
            {
              p = { x = 0.; y = 0. };
              r = 10. *. scale;
              n_segments = 5;
              rot_angle = Float.pi;
            };
        render = true;
      }
  in
  let t1 =
    Fractal.
      {
        s =
          P
            {
              p = { x = 0.; y = 0. };
              r = 10. *. scale;
              n_segments = 10;
              rot_angle = Float.pi /. 2.;
            };
        render = true;
      }
  in
  let t2 =
    Fractal.
      {
        s =
          P
            {
              p = { x = 0.; y = 0. };
              r = 1. *. scale;
              n_segments = 3;
              rot_angle = Float.pi /. 2.;
            };
        render = true;
      }
  in
  let r4 =
    Fractal.
      {
        target = t2;
        n = 10;
        rot_offset = Float.pi /. 5.;
        d = 1.8;
        children = [];
      }
  in
  let r3 =
    Fractal.
      {
        target = t2;
        n = 10;
        rot_offset = Float.pi /. 24.;
        d = 1.15;
        children = [];
      }
  in
  let r2 =
    Fractal.
      {
        target = t2;
        n = 10;
        rot_offset = -.Float.pi /. 24.;
        d = 1.15;
        children = [];
      }
  in
  let r1 =
    Fractal.{ target = t1; n = 10; rot_offset = 0.; d = 1.; children = [] }
  in
  let shapes = Fractal.v t0 [ r1; r2; r3; r4 ] in
  List.iter (Draw.v cf ~lw:4.0) shapes

let () =
  let width = 1000 in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:width in
  let cf = Conf.init surface width in
  Cairo.set_source_rgb cf.cr 0.0 0.0 0.0;
  Cairo.paint cf.cr;
  draw cf;
  Cairo.PNG.write surface "output.png"
