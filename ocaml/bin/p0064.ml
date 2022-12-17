(* Project Euler: Problem 64 *)

(*
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
     a{n} = floor( (sqrt(N)+b{n}) / c{n} ) = (floor(sqrt(N) + b{n}) / c{n}
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}

     a{0} = sqrt(N), b{0} = 0, c{0} = 1

   -->
     cycle_lst = [((b{n}, c{n}), n); ...; ((b{1}, c{1}), 1); ((b{0}, c{0}), 0)]
 *)

open Core

let eq_int_pair (x1, x2) (y1, y2) =
  x1 = y1 && x2 = y2

let get_cyclic_period num =
  let isqrt_N = Euler.Math.isqrt num in
  if num = isqrt_N * isqrt_N then
    0
  else
    let rec loop b c i lst =
      let a = (isqrt_N + b) / c in
      let next_b = a * c - b in
      let next_c = (num - next_b * next_b) / c in

      match List.Assoc.find lst (next_b, next_c) ~equal:eq_int_pair with
      | Some v -> (i + 1) - v
      | None -> loop next_b next_c (succ i) (((next_b, next_c), (i + 1)) :: lst)
    in
    loop 0 1 0 [(0, 1), 0]

let solve () =
  let rec loop m result =
    if m = 0 then
      result
    else
      match get_cyclic_period m with
      | x when x mod 2 = 1 -> loop (pred m) (succ result)
      | _ -> loop (pred m) result
  in
  loop 10_000 0

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec