(* Project Euler: Problem 91 *)

(*
  0 <= x1,x2,y1,y2 <= 50
  P(x1,y1), Q(x2,y2)

  case #1:
    1-1) the right angle is O(0,0)
      P(x1>0,0) and Q(0,y2>0)
      --> 50 * 50
    1-2) the right angle is on the x-axis [P(x1>0,y1=0)]
      P(x1>0,0) and Q(x2=x1,y2>0)
      --> 50 * 50
    1-3) the right angle is on the y-axis [Q(x1=0,y2>0)]
      P(x1>0,y1=y2) and Q(0,y2>0)
      --> 50 * 50

  case #2:
    2-1) P(x1,y1) is the right angle and Qi(x_i>x1,y_i<y1)
      Y
       | P(x1,y1)
       |   #   Q1(a1,b1)
       |  .       *   Q2(a2,b2)
       | .               *
       |.                       *
      -O------------------------------------- X
                                            *

      --> min((y1 / (x / gcd(x1,y1))), ((50-x1) / (y / gcd(x1,y1))))

    2-2) P(x1,y1) is the right angle and Q(x2<x1,y2>y1)
      same qty as case 2-1.  [mirror on the y=x line]
 *)

open Core

let compute x_size y_size =
  let case_1 x y = x * y * 3 in
  let case_2 x y =
    let acc = ref 0 in
    for x1 = 1 to x do
      for y1 = 1 to y do
        let m = Euler.Math.gcd x1 y1 in
        acc := !acc + (min ((y1 * m) / x1) ((x - x1) * m / y1))
      done
    done;
    !acc * 2
  in

  case_1 x_size y_size + case_2 x_size y_size
;;


let solve () = compute 50 50 |> Int.to_string

(* Test *)

let%test_unit "2, 2" = [%test_eq: int] (compute 2 2) 14
let%test_unit "50, 50" = [%test_eq: int] (compute 50 50) 14234
