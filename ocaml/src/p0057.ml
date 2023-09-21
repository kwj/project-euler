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

let compute limit =
  let z_ndigits n = Z.to_string n |> String.length in
  Sequence.unfold
    ~init:Z.(~$3, ~$2)
    ~f:(fun (n, d) -> Some ((n, d), Z.((d * ~$2) + n, d + n)))
  |> Fun.flip Sequence.take limit
  |> Sequence.count ~f:(fun (n, d) -> (z_ndigits n) > (z_ndigits d))
;;

let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "8" = [%test_eq: int] (compute 8) 1
let%test_unit "1_000" = [%test_eq: int] (compute 1_000) 153

