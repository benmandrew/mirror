open Mirror
open Geometry
open Fractal

let a cf =
  let scale = 24. in
  let t0 =
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
    { target = t2; n = 10; rot_offset = Float.pi /. 5.; d = 1.8; children = [] }
  in
  let r3 =
    {
      target = t2;
      n = 10;
      rot_offset = Float.pi /. 24.;
      d = 1.15;
      children = [];
    }
  in
  let r2 =
    {
      target = t2;
      n = 10;
      rot_offset = -.Float.pi /. 24.;
      d = 1.15;
      children = [];
    }
  in
  let r1 = { target = t1; n = 10; rot_offset = 0.; d = 1.; children = [] } in
  let shapes = v t0 [ r1; r2; r3; r4 ] in
  Set.iter (Draw.v cf ~lw:4.0) shapes

let b cf =
  let t =
    {
      s =
        P { p = { x = 0.; y = 0. }; r = 150.; n_segments = 10; rot_angle = 0. };
      render = false;
    }
  in
  let r2 =
    {
      target = { t with render = true };
      n = 5;
      rot_offset = 0.;
      d = 2.0;
      children = [];
    }
  in
  let r1 =
    { target = t; n = 10; rot_offset = 0.; d = 2.0; children = [ r2 ] }
  in
  let r0 =
    { target = t; n = 10; rot_offset = 0.; d = 2.0; children = [ r1 ] }
  in
  Set.iter (Draw.v cf ~lw:3.0) @@ v t [ r0 ]

let c cf =
  let t = { s = C { p = { x = 0.; y = 0. }; r = 200. }; render = false } in
  let r3 =
    {
      target = { t with render = true };
      n = 6;
      rot_offset = 0.;
      d = 1.0;
      children = [];
    }
  in
  let r2 = { target = t; n = 6; rot_offset = 0.; d = 1.0; children = [ r3 ] } in
  let r1 = { target = t; n = 6; rot_offset = 0.; d = 1.0; children = [ r2 ] } in
  let r0 = { target = t; n = 6; rot_offset = 0.; d = 1.0; children = [ r1 ] } in
  Set.iter (Draw.v cf ~lw:10.0) @@ v t [ r0 ]

let d cf =
  let t =
    {
      s =
        P
          {
            p = { x = 0.; y = 0. };
            r = 150.;
            n_segments = 6;
            rot_angle = -.Float.pi /. 4.;
          };
      render = false;
    }
  in
  let r2 =
    {
      target = { t with render = true };
      n = 6;
      rot_offset = 0.;
      d = 2.25;
      children = [];
    }
  in
  let r1 =
    { target = t; n = 6; rot_offset = 0.; d = 2.25; children = [ r2 ] }
  in
  let r0 =
    { target = t; n = 12; rot_offset = 0.; d = 2.25; children = [ r1 ] }
  in
  Set.iter (Draw.v cf ~lw:5.0) @@ v t [ r0 ]

let e cf =
  let scale = 3.25 in
  let t0 =
    {
      s =
        P
          {
            p = { x = 0.; y = 0. };
            r = 150. *. scale;
            n_segments = 5;
            rot_angle = -.Float.pi /. 2.;
          };
      render = true;
    }
  in
  let t1 =
    {
      s =
        P
          {
            p = { x = 0.; y = 0. };
            r = 150. *. scale;
            n_segments = 10;
            rot_angle = -.Float.pi /. 2.;
          };
      render = true;
    }
  in
  let t2 =
    {
      s =
        P
          {
            p = { x = 0.; y = 0. };
            r = 60. *. scale;
            n_segments = 7;
            rot_angle = -.Float.pi /. 2.;
          };
      render = true;
    }
  in
  let t3 =
    {
      s =
        P
          {
            p = { x = 0.; y = 0. };
            r = 50. *. scale;
            n_segments = 5;
            rot_angle = -.Float.pi /. 4.;
          };
      render = true;
    }
  in
  let r2 = { target = t3; n = 0; rot_offset = 0.; d = 2.; children = [] } in
  let r1 =
    { target = t2; n = 20; rot_offset = 0.; d = 2.; children = [ r2 ] }
  in
  let r0 =
    {
      target = t1;
      n = 10;
      rot_offset = -.twopi /. 4.;
      d = 2.;
      children = [ r1 ];
    }
  in
  Set.iter (Draw.v cf ~lw:2.) @@ v t0 [ r0 ]

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
  run "d" d;
  run "e" e
