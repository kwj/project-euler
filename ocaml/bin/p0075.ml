(* Project Euler: Problem 75 *)

(*
  a^2 + b^2 = c^2    [(a, b), (a, c) and (b, c) are coprime integers]

    a  b  c^2 -> c
    -------------------------
    E  E  E      E  <- NG [a, b and c are coprime integers]
    E  O  O      O
    O  E  O      O
    O  O  E      E  <- NG
     because... write a=2s+1, b=2t+1, c=2u
       a^2 + b^2 = 4s^2 + 4s + 1 + 4t^2 + 4t + 1
                 = 2(2s^2 + 2s + 2t + 1)
       c^2 = 4u^2 = 2(2s^2 + 2s + 2t + 1)
       --> 2u^2 = 2(s^2 + s + t) + 1  <-- contradiction

  -->
    a^2/c^2 + b^2/c^2 = 1
      x = a/c, y = b/c
    x^2 + y^2 = 1

  x^2 + y^2 = 1, y = -kx + 1 [0<k<1, k is rational number]
  -->
    x^2 + (-kx + 1)^2 = x^2 + k^2*x^2 - 2kx + 1 = 1
    x^2(k^2 + 1) = 2kx
    x = 2k / (k^2 + 1)

    y = -k * (2k / (k^2 + 1)) + 1
      = 1 - (2k^2 / (k^2 + 1)
      = ((K^2 + 1) - 2k^2) / (K^2 + 1)
      = (1 - k^2) / (k^2 + 1)

  k is rational number = n/m  [GCD(m,n)=1]
  -->
    x = (2 * n/m) / (n^2/m^2 + 1)
      = 2mn / (n^2 + m^2)
    y = (1 - n^2/m^2) / (n^2/m^2 + 1)
      = (m^2 - n^2) / (n^2 + m^2)

    a/c =         2mn / (n^2 + m^2)
    b/c = (m^2 - n^2) / (n^2 + m^2)
  -->
    a = 2mn, b = m^2 - n^2, c = m^2 + n^2  [m>n]

  ==>
    L = i(a + b + c) 
      = i(2mn + 2m^2)
      = i * 2m(m + n)  [i>0, m>n, gcd(m,n)=1]

    2 * max(m)^2 < L
    max(m) < sqrt(L/2)
 *)

(* ---------------------------------------------------------------- *)

open Core

let solve limit =
  let upper_m = Euler.Math.isqrt (limit / 2) in
  let cnt_arr = Array.create ~len:(limit + 1) 0 in
  let rec loop_i mn i =
    let l = i * mn in
    if l > limit then
      ()
    else (
      cnt_arr.(l) <- cnt_arr.(l) + 1;
      loop_i mn (succ i)
    )
  in
  for m = 2 to upper_m do
    for n = 1 to m - 1 do
      if (m + n) mod 2 = 1 && Euler.Math.gcd m n = 1 then
        loop_i (2 * m * (m + n)) 1
    done
  done;
  Array.fold cnt_arr ~init:0 ~f:(fun acc i -> if i = 1 then (succ acc) else acc)

let exec () =
  Int.to_string (solve 1_500_000)

let () = Euler.Task.run exec
