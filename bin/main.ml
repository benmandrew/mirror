open Mirror
open Geometry

let draw cf =
  let p =
    {
      p = { x = 0.; y = 0. };
      r = 150.;
      n_segments = 5;
      rot_angle = -.Float.pi /. 2.;
    }
  in
  let ps = fractal_p_repeat p ~ns:[ 10; 10; 5 ] ~d:2. ~r:1. in
  List.iter (Draw.polygon cf ~lw:2.0) ps

let () =
  let width = 1000 in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:width in
  let cf = Conf.init surface width in
  Cairo.set_source_rgb cf.cr 0.0 0.0 0.0;
  Cairo.paint cf.cr;
  draw cf;
  Cairo.PNG.write surface "output.png"
