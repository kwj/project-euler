(* Project Euler: Problem 57 *)

(*
  use recurrence relation:
    sqrt(2) = 1 + sqrt(2) - 1
            = 1 + 1 / ( 1 / (sqrt(2) - 1) )
            = 1 + 1 / ( (sqrt(2) + 1) / (2 - 1) )
            = 1 + 1 / (1 + sqrt(2))
    -->
    a{1} = 1 + 1/2
    a{n} = 1 + 1/(1 + a{n-1})    [n>1]

  assume that b{n}/c{n} = a{n}
    b{1}/c{1} = 1 + 1/2 = 3/2
    b{n}/c{n} = 1 + 1/(1 + b{n-1}/c{n-1})
              = 1 + 1/((c{n-1) + b{n-1})/c{n-1})
              = 1 + c{n-1}/(c{n-1) + b{n-1})
              = (c{n-1) + b{n-1} + c{n-1))/(c{n-1) + b{n-1})
              = (2 * c{n-1} + b{n-1}) / (c{n-1) + b{n-1})
 *)

open Core

(* -- method 1 -- *)
(* using arbitrary-precision arithmetic library *)
let solve_1 num =
  let num_of_digits n = String.length (Z.to_string n) in
  Sequence.take (Sequence.unfold ~f:(fun (n, d) -> Some((n, d), Z.((d * ~$2 + n, d + n)))) ~init:Z.(~$3, ~$2))
                num
  |> Sequence.filter ~f:(fun (n, d) -> num_of_digits n > num_of_digits d)
  |> Sequence.length

(* -- method 2 -- *)
(* using list instead of arbitrary-precision arithmetic library *)
let solve_2 num =
  let module U = Euler.Util in
  let num_of_digits n_lst = List.length n_lst in
  let rec loop cnt bn cn result =
    if cnt = 0 then
      result
    else (
      if num_of_digits bn > num_of_digits cn then
        loop (pred cnt) (U.add_nlst (U.mul_nlst cn 2) bn) (U.add_nlst cn bn) (succ result)
      else
        loop (pred cnt) (U.add_nlst (U.mul_nlst cn 2) bn) (U.add_nlst cn bn) result
    )
  in
  loop num (U.nlst_of_int 3) (U.nlst_of_int 2) 0

let exec () =
  sprintf "%d: (w/ Zarith module)\n%d: (w/o Zarith module)" (solve_1 1_000) (solve_2 1_000)

let () = Euler.Task.run exec
