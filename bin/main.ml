open Mirror
open Geometry
open Fractal

let draw cf =
  let (t, r) = Conf.parse "bin/input.json" in
  Set.iter (Draw.v cf ~lw:4.0) @@ v t r

let () =
  let width = 1000 in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:width ~h:width in
  let cf = Conf.init surface width in
  let Conf.{ r; g; b } = Conf.bg in
  Cairo.set_source_rgb cf.cr r g b;
  Cairo.paint cf.cr;
  draw cf;
  Cairo.PNG.write surface "output.png"
