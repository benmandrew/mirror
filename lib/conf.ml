type t = { w : int; wf : float; hwf : float; cr : Cairo.context }

let init surface width =
  {
    w = width;
    wf = float_of_int width;
    hwf = float_of_int width /. 2.;
    cr = Cairo.create surface;
  }
