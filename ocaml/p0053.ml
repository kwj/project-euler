(* Project Euler: Problem 53 *)

(*
  >>> import math
  >>> math.log10(math.factorial(100))
  157.97000365471578
  >>> math.log10(math.factorial(50))
  64.48307487247203

  Since it overflows, I need to use arbitrary-precision arithmetic library or another method.
 *)

(* ---------------------------------------------------------------- *)

(* -- method 1 -- *)
(* using arbitrary-precision arithmetic library *)
module Facttbl : sig
  val make : int -> Z.t array
  val get : Z.t array -> int -> Z.t
end = struct
  let make n =
    let rec aux i lst =
      if i > n then
        Array.of_list (List.rev lst)
      else
        aux (succ i) ((Z.((of_int i) * List.hd lst)) :: lst)
    in
    aux 1 [Z.one]

  let get tbl i = tbl.(i)
end

let solve_1 num =
  let ftbl = Facttbl.make num in
  let is_over cn cr value =
    (* C(n,r) = n! / r! / (n-r)! > value ? *)
    let tmp = cn - cr in
    if Z.(((Facttbl.get ftbl cn) / (Facttbl.get ftbl cr)) / (Facttbl.get ftbl tmp) > Z.of_int value) then
      true
    else
      false
  in
  let rec aux n result =
    if n < 1 then
      result
    else
      let rec aux' r cnt =
        if r < 1 then
          cnt
        else
          if is_over n r 1_000_000 then
            aux' (pred r) (cnt + 1)
          else
            aux' (pred r) cnt
      in
      aux (pred n) (result + (aux' n 0))
  in
  aux num 0


(* -- method 2 -- *)
(*
  using Pascal's triangle
    C(n,r) = C(n-1,r-1) + C(n-1,r)
    C(n,0) = C(n,n) = 1
 *)
let solve_2 ulimit =
  let p_tri = Array.make_matrix (ulimit + 1) (ulimit + 1) 0 in
  let rec aux n result =
    if n > ulimit then
      result
    else
      let rec aux' r cnt =
        if r < 1 then
          cnt
        else (
          p_tri.(n).(r) <- (p_tri.(n-1).(r-1) + p_tri.(n-1).(r));
          if p_tri.(n).(r) > 1_000_000 then (
            p_tri.(n).(r) <- 1_000_000;      (* prevent overflow *)
            aux' (pred r) (cnt + 1)
          ) else
            aux' (pred r) cnt
        )
      in
      aux (succ n) (result + (aux' n 0))
  in
  for i = 0 to ulimit do
    p_tri.(i).(0) <- 1;
  done;
  aux 1 0

let () =
  Printf.printf "Answer: %d  (w/ Zarith module)\n" (solve_1 100);
  Printf.printf "Answer: %d  (w/o Zarith module)\n" (solve_2 100)
