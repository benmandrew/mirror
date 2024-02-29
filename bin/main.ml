type pos = { x : float; y : float }

let add_pos { x = x0; y = y0 } { x = x1; y = y1 } =
  { x = x0 +. x1; y = y0 +. y1 }

type circle = { p : pos; r : float }

(* type arc = {
     c : circle;
     a1 : float;
     a2 : float;
   } *)

type polygon = { p : pos; r : float; rot_angle : float; n_segments : int }

(* type shape =
   | Circle of circle
   | Polygon of polygon *)

let twopi = 2. *. Float.pi
let w = 1000
let wf = float_of_int w
let hwf = wf /. 2.
let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w ~h:w
let cr = Cairo.create surface

(** [angles ~closed ~offset n] generates a list of equally spaced angles
    (in radians), starting from [offset]. If [closed = true] then we
    repeat the first angle at the end of the list to close the loop,
    and thus have [n+1] elements. Otherwise, we have [n] elements. *)
let angles ?(offset = 0.) n =
  List.init n (fun i -> (float_of_int i *. twopi /. float_of_int n) +. offset)

let fractal_c (c : circle) ~n ~d ~r =
  let angles = angles n in
  let f a =
    let offset = { x = c.r *. d *. cos a; y = c.r *. d *. sin a } in
    { p = add_pos c.p offset; r = r *. c.r }
  in
  List.map f angles

(** Generate [n] new polygons arranged around [p].
    - [d] is the relative distance offset of each polygon's centre
    - [r] is the relative new radius of each polygon
      as a proportion of [p.r] *)
let fractal_p (p : polygon) ~n ~d ~r =
  let angles = angles n in
  let f a =
    let offset = { x = p.r *. d *. cos a; y = p.r *. d *. sin a } in
    {
      p = add_pos p.p offset;
      r = r *. p.r;
      n_segments = p.n_segments;
      rot_angle = p.rot_angle +. a +. (-.twopi /. 4.);
    }
  in
  List.map f angles

(** Recursively apply the fractal operation, with each element of
    [ns] being the number of newly generated polygons per polygon
    of the previous iteration. *)
let fractal_p_repeat (p : polygon) ~ns ~d ~r =
  List.fold_left
    (fun ps n ->
      let pss = List.map (fun p -> fractal_p p ~n ~d ~r) ps in
      List.concat pss)
    [ p ] ns

let draw_circle cr ?(lw = 2.0) (c : circle) =
  Cairo.set_line_width cr lw;
  Cairo.set_source_rgb cr 1. 1. 1.;
  Cairo.arc cr (c.p.x +. hwf) (c.p.y +. hwf) ~r:c.r ~a1:0. ~a2:twopi;
  Cairo.stroke cr

let draw_polygon cr ?(lw = 2.0) (p : polygon) =
  let angles = angles ~offset:p.rot_angle p.n_segments in
  Cairo.set_line_width cr lw;
  Cairo.set_source_rgb cr 1. 1. 1.;
  let offsets =
    List.map (fun a -> { x = p.r *. cos a; y = p.r *. sin a }) angles
  in
  let f offset =
    let pos = add_pos p.p offset in
    Cairo.line_to cr (pos.x +. hwf) (pos.y +. hwf)
  in
  List.iter f offsets;
  Cairo.Path.close cr;
  Cairo.stroke cr

let example_1 () =
  let p =
    {
      p = { x = 0.; y = 0. };
      r = 150.;
      n_segments = 5;
      rot_angle = -.Float.pi /. 2.;
    }
  in
  let ps = fractal_p_repeat p ~ns:[ 10; 5 ] ~d:2. ~r:1. in
  List.iter (draw_polygon cr ~lw:15.) ps

let example_2 () =
  let p =
    {
      p = { x = 0.; y = 0. };
      r = 150.;
      n_segments = 5;
      rot_angle = -.Float.pi /. 2.;
    }
  in
  let ps = fractal_p_repeat p ~ns:[ 10; 10; 5 ] ~d:2. ~r:1. in
  List.iter (draw_polygon cr ~lw:2.0) ps

let example_3 () =
  let c = { p = { x = 0.; y = 0. }; r = 200. } in
  draw_circle cr c;
  List.iter (draw_circle cr) @@ fractal_c c ~n:6 ~d:1. ~r:1.

let () =
  ignore (example_2, example_3);
  Cairo.set_source_rgb cr 0.0 0.0 0.0;
  Cairo.paint cr;
  example_1 ();
  Cairo.PNG.write surface "output.png"
