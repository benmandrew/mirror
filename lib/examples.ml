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
