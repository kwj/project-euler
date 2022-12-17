(* Project Euler: Problem 100 *)

(*
  https://en.wikipedia.org/wiki/Pell%27s_equation#The_negative_Pell_equation

  number of blue disks: a
  number of total disks: b

  P(BB) = a/b * (a-1)/(b-1) = 1/2
  --> 2a(a-1) = b(b-1)
      2a^2 - 2a = b^2 - b
      4 * (2a^2 - 2a) = 4 * (b^2 - b)
      2 * (2a - 1)^2 - 2 = (2b - 1)^2 - 1
      (2b-1)^2 - 2(2a-1)^2 = -1

      x=2b-1, y=2a-1  [a = (y+1)/2]
      --> x^2 - 2y^2 = -1  [x,y are odd numbers] --- a negative pell's equation
          (x,y) = (1,1) -> 1 - 2*1 = -1

          (1+1*sqrt(2))^3 = (3+2*sqrt(2)) * (1+1*sqrt(2))
          --> x{n} = 3 * x{n-1} + 2 * 2 * y{n-1}
              y{n} = 2 * x{n-1} + 3 * y{n-1}

              if both x{1}, y{1} are odd, all x{n}, y{n} are odd too.
 *)

(* ---------------------------------------------------------------- *)

let solve low =
  let x1, y1 = 1, 1 in
  let border = 2 * low - 1 in      (* x = 2b - 1 *)
  let rec loop x y =
    if x > border then
      (y + 1) / 2      (* a = (y + 1) /2 *)
    else
      loop (3 * x + 4 * y) (2 * x + 3 * y)
  in
  loop x1 y1

let () =
  Printf.printf "Answer: %d\n" (solve 1_000_000_000_000)
