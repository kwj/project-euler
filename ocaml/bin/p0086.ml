(* Project Euler: Problem 86 *)

(*
  1 <= a, b, c <= M

  we can ignore rotations. there is only one case to consider.
    1 <= a <= b <= c <= M
     --> 2 <= a + b <= 2c

      +--------F
      |        |      * sqrt(c^2 + (a+b)^2) must be an integer
      |--------|
      |        | a+b >= 2
      |        |
      S--------+
           c

  when a+b <= c <= M
    write a+b = x
      (a, b) = (1, x-1), (2, x-2), ..., (x-1, 1)
    however, because a<=b
      num of (a,b) = floor(x/2) = floor((a+b)/2)

  when a+b > c
      num of (a,b) = floor((a+b)/2) - ((a+b-1) - c)

      example: c=10, a+b=15
        (a,b) = (1,14), ..., (5,10), (6,9), (7,8), ..., (14,1)
                             ####################
                ^^^ b>c ^^^ = (a+b-1) - c
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)

      example: c=10, a+b=16
        (a,b) = (1,15), ..., (6,10), (7,9), (8,8), ..., (14,1)
                             ####################
                ^^^ b>c ^^^ = (a+b-1) - c
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ = floor((a+b)/2)
 *)

open Core

let solve limit =
  let rec loop c acc =
    if acc > limit then
      pred c
    else
      let rec aux sum_ab acc =
        if sum_ab < 2 then
          acc
        else (
          let tmp = (c * c) + (sum_ab * sum_ab) in
          let sq_tmp = Euler.Math.isqrt tmp in
          if tmp <> sq_tmp * sq_tmp then
            aux (pred sum_ab) acc
          else (
            if sum_ab <= c then
              aux (pred sum_ab) (acc + sum_ab / 2)
            else
              aux (pred sum_ab) (acc + sum_ab / 2 - ((sum_ab - 1) - c))
          )
        )
      in
      loop (succ c) (acc + aux (c * 2) 0)
  in
  loop 1 0

let exec () =
  Int.to_string (solve (1_000_000))

let () = Euler.Task.run exec
