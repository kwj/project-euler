(* Project Euler: Problem 53 *)

(*
  >>> import math
  >>> math.log10(math.factorial(100))
  157.97000365471578
  >>> math.log10(math.factorial(50))
  64.48307487247203

  Since the above result, I need to use arbitrary-precision arithmetic library or another method.
  Therefore, I use Pascal's triangle formula.
    C(n,r) = C(n-1,r-1) + C(n-1,r)
    C(n,0) = C(n,n) = 1
 *)

open Core

let get_next_row lst =
  let rec aux acc = function
    | x :: [] -> List.map (x :: acc)
                   ~f:(fun n -> if n > 1_000_000 then 1_000_001 else n)  (* overflow prevention *)
    | x :: (y :: _ as xs) -> aux ((x + y) :: acc) xs
    | [] -> failwith "not reached"
  in
  aux [] (0 :: lst)

let solve () =
  let rec aux n acc lst =
    if n > 100 then
      acc
    else
      let next_row = get_next_row lst in
      aux (succ n) ((List.count ~f:(fun elm -> elm > 1_000_000) next_row) + acc) next_row
  in
  aux 1 0 [1]

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec
