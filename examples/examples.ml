open Mirror
open Geometry
open Fractal

let a cf =
  let (t, r) = Conf.parse "examples/a.json" in
  Set.iter (Draw.v cf ~lw:4.0) @@ v t r

let b cf =
  let (t, r) = Conf.parse "examples/b.json" in
  Set.iter (Draw.v cf ~lw:3.0) @@ v t r

let c cf =
  let (t, r) = Conf.parse "examples/c.json" in
  Set.iter (Draw.v cf ~lw:10.0) @@ v t r

let d cf =
  let (t, r) = Conf.parse "examples/d.json" in
  Set.iter (Draw.v cf ~lw:5.0) @@ v t r

let e cf =
  let (t, r) = Conf.parse "examples/e.json" in
  Set.iter (Draw.v cf ~lw:2.) @@ v t r

open Mirror

let run label f =
  let width = 1000 in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:width in
  let cf = Conf.init surface width in
  let Conf.{ r; g; b } = Conf.bg in
  Cairo.set_source_rgb cf.cr r g b;
  Cairo.paint cf.cr;
  f cf;
  Cairo.PNG.write surface @@ Printf.sprintf "examples/output_%s.png" label

let () =
  run "a" a;
  run "b" b;
  run "c" c;
  run "d" d;
  run "e" e
