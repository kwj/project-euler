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
     a{n+1} = floor((sqrt(N)+b{n+1}) / c{n+1})

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

(* ---------------------------------------------------------------- *)

(* 
  Please see the following page.
    https://en.wikipedia.org/wiki/Pell%27s_equation#Solutions
 *)
let is_pell n x y =
  let open Z in
  let tmp = x * x - (of_int n) * y * y in
  if tmp = Z.one then      (* X^2 - D*Y^2 = 1 *)
    Some (x, y)
  else
    if tmp = Z.minus_one then      (* X^2 - D*Y^2 = - 1 *)
      Some (x * x + (of_int n) * y * y, (of_int 2) * x * y)
    else
      None

let find_pair num =
  let sqrt_num = Float.(to_int (sqrt (of_int num))) in
  let rec loop a b c n1 n2 d1 d2 =
    let next_b = a * c - b in
    let next_c = (num - next_b * next_b) / c in
    let next_a = (sqrt_num + next_b) / next_c in
    let next_n = Z.((of_int next_a) * n1 + n2) in
    let next_d = Z.((of_int next_a) * d1 + d2) in
    match is_pell num next_n next_d with
    | None -> loop next_a next_b next_c next_n n1 next_d d1
    | Some v -> v
  in
  loop sqrt_num 0 1 Z.(of_int sqrt_num) Z.one Z.one Z.zero      (* a0, b0, c0, x0, x{-1}, y0, y{-1} *)

let solve () =
  let rec loop m max_x d =
    if m > 1_000 then
      d
    else
      if Float.(is_integer (sqrt (of_int m))) then
        loop (succ m) max_x d
      else
        let x, _ = find_pair m in
        if x > max_x then
          loop (succ m) x m
        else
          loop (succ m) max_x d
  in
  loop 2 Z.zero 0

let () =
  Printf.printf "Answer: %d\n" (solve())
