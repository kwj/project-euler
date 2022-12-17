(* Project Euler: Problem 66 *)

(*
  X^2 - N * Y^2 = 1
  -----------------

  This equation is called Pell's equation, but I didn't know that.
  So I wrote a solution by referring Wikipedia.

    https://en.wikipedia.org/wiki/Pell%27s_equation


            sqrt(N) + b0        1              1
  sqrt(N) = ------------ = a0 + --,  x1 = a1 + --, ...
                 c0             x1             x2

                  c0             c0(sqrt(N) - (b0 - a0c0))
    x1 = --------------------- = -------------------------
         sqrt(N) + (b0 - a0c0)       N - (b0 - a0c0)^2

         sqrt(N) + (a0c0 - b0)   sqrt(N) + b1         1
       = --------------------- = ------------- = a1 + --
           N - (a0c0 - b0)^2          c1              x2
           -----------------
                  c0
   -->
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}
     a{n+1} = floor((sqrt(N)+b{n+1}) / c{n+1}) = (floor(sqrt(N)) + b{n+1}) / c{n+1}

       a{0} = floor(sqrt(N)), b{0} = 0, c{0} = 1


  write A{0}, A{1}, A{2}, ... 
           x{0}                          x{1}                      1         x{2}
    a{0} = ---- = A{0},  a{0} + 1/a{1} = ---- = A{1},  a{0} + ------------ = ---- = A{2},  ...
           y{0}                          y{1}                         1      y{2}
                                                              a{1} + ----
                                                                     a{2}
                                              [x{0} = a{0}, y{0} = 1]
   -->
        n=0: -> x{0} = a{0}, y{0} = 1
        n=1: -> x{1} = a{0}*a{1} + 1, y{1} = a{1}
 
        n=2: -> x{2}/y{2}
                     = (a{0}*a{1}*a{2} + a{0} + a{2}) / (a{1}a{2} + 1)
 
                       a{2}*(a{0}*a{1} + 1) + a{0}   a{2}*x{1} + a{0}
                     = --------------------------- = ----------------
                            a{2}*a{1} + 1            a{2}*y(1) + 1

                       a{2}*x{1} + x{0}
                     = ----------------
                       a{2)*y{1} + y{0}

                                    a{k}*x{k-1} + x{k-2}
     assume that A{k} = x{k}/y{k} = --------------------  [k>=2]
                                    a{k}*y{k-1} + y{k-2}

                                 ((a{k}*a{k+1} + 1)/a{k+1})*x{k-1} + x{k-2}
        A{k+1} = x{k+1}/y{k+1} = -----------------------------------------
                                 ((a{k}*a{k+1} + 1)/a{k+1})*y{k-1} + y{k-2}

                                 (a{k}*a{k+1} + 1)*x{k-1} + x{k-2}*a{k+1}
                               = -----------------------------------------
                                 (a{k}*a{k+1} + 1)*y{k-1} + y{k-2}*a{k+1}

                                 a{k+1}(a{k}*x{k-1} + x{k-2}) + x{k-1}
                               = -------------------------------------
                                 a{k+1}(a{k}*y{k-1} + y{k-2}) + y{k-1}

                                 a{k+1}*x{k} + x{k-1}
                               = --------------------
                                 a{k+1}*y{k} + y{k-1}                              
      -->
        x{k+1} = a{k+1} * x{k} + x{k-1}
        y{k+1} = a{k+1} * y{k} + y{k-1} 
 *)

open Core

let isqrt = Euler.Math.isqrt

(* 
  Please see the following page.
    https://en.wikipedia.org/wiki/Pell%27s_equation#Solutions
 *)
let is_pell n x y =
  let open Z in
  let tmp = x * x - (of_int n) * y * y in
  if Z.(equal tmp one) then
    Some (x, y)                                         (* when X^2 - D*Y^2 = 1 *)
  else
    if Z.(equal tmp minus_one) then
      Some (x * x + (of_int n) * y * y, ~$2 * x * y)    (* when X^2 - D*Y^2 = -1 *)
    else
      None

let find_pair n isqrt_N =
  let rec loop a b c x2 x1 y2 y1 =
    let next_b = a * c - b in
    let next_c = (n - next_b * next_b) / c in
    let next_a = (isqrt_N + next_b) / next_c in
    let next_x = Z.((of_int next_a) * x2 + x1) in
    let next_y = Z.((of_int next_a) * y2 + y1) in

    match is_pell n next_x next_y with
    | Some v -> v
    | None -> loop next_a next_b next_c next_x x2 next_y y2
  in
  loop isqrt_N 0 1 Z.(of_int isqrt_N) Z.one Z.one Z.zero    (* a0, b0, c0, x0, x{-1}, y0, y{-1} *)

let solve () =
  let rec loop d max_x max_d =
    if d > 1_000 then
      max_d
    else
      let isqrt_D = isqrt d in
      if d = isqrt_D * isqrt_D then
        loop (succ d) max_x max_d
      else
        let x, _ = find_pair d isqrt_D in
        if Z.(gt x max_x) then
          loop (succ d) x d
        else
          loop (succ d) max_x max_d
  in
  loop 2 Z.zero 0

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
