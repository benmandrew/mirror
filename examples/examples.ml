open Mirror
open Geometry

let a cf =
  let p =
    {
      p = { x = 0.; y = 0. };
      r = 150.;
      n_segments = 5;
      rot_angle = -.Float.pi /. 2.;
    }
  in
  let ps = fractal_p_repeat p ~ns:[ 10; 5 ] ~d:2. ~r:1. in
  List.iter (Draw.polygon cf ~lw:15.) ps

let b cf =
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

let c cf =
  let c = { p = { x = 0.; y = 0. }; r = 200. } in
  Draw.circle cf c;
  List.iter (Draw.circle cf) @@ fractal_c c ~n:6 ~d:1. ~r:1.

let d cf =
  let p =
    {
      p = { x = 0.; y = 0. };
      r = 150.;
      n_segments = 6;
      rot_angle = -.Float.pi /. 4.;
    }
  in
  let ps = fractal_p_repeat p ~ns:[ 12; 6; 6 ] ~d:2.25 ~r:1. in
  List.iter (Draw.polygon cf ~lw:2.0) ps

open Mirror

let run label f =
  let width = 1000 in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:width in
  let cf = Conf.init surface width in
  Cairo.set_source_rgb cf.cr 0.0 0.0 0.0;
  Cairo.paint cf.cr;
  f cf;
  Cairo.PNG.write surface @@ Printf.sprintf "examples/output_%s.png" label

let () =
  run "a" a;
  run "b" b;
  run "c" c;
  run "d" d
