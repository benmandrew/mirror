type t = { w : int; wf : float; hwf : float; cr : Cairo.context }

type rgb = { r : float; g : float; b : float }

let bg = { r = 0.0; g = 0.0; b = 0.0 }
let fg = { r = 1.0; g = 1.0; b = 1.0 }

let init surface width =
  {
    w = width;
    wf = float_of_int width;
    hwf = float_of_int width /. 2.;
    cr = Cairo.create surface;
  }

let parse file =
  let res =
    Fractal.of_yojson @@
    In_channel.with_open_text file Yojson.Safe.from_channel
  in
  match res with
  | Ok r -> r
  | Error m -> failwith m
