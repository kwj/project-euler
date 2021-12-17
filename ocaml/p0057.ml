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

(* ---------------------------------------------------------------- *)

(* -- method 1 -- *)
(* using arbitrary-precision arithmetic library *)
let solve_1 num =
  let ndigits n = String.length (Z.to_string n) in
  let rec loop cnt bn cn result =
    if cnt = 0 then
      result
    else
      if ndigits bn > ndigits cn then
        loop (pred cnt) Z.((of_int 2) * cn + bn) Z.(cn + bn) (succ result)
      else
        loop (pred cnt) Z.((of_int 2) * cn + bn) Z.(cn + bn) result
  in
  loop num (Z.of_int 3) (Z.of_int 2) 0

(* -- method 2 -- *)
(* using list instead of arbitrary-precision arithmetic library *)
let solve_2 num =
  let rec add_nlst l1 l2 =
    let rec aux l1 l2 carry acc =
      match l1, l2 with
      | hd1 :: tl1, hd2 :: tl2 ->
         aux tl1 tl2 ((hd1 + hd2 + carry) / 10) (((hd1 + hd2 + carry) mod 10) :: acc)
      | [], [] when carry > 0 ->
         List.rev (carry :: acc)
      | _ ->
         List.rev acc
    in
    let diff = (List.length l1) - (List.length l2) in
    if diff = 0 then
      aux l1 l2 0 []
    else
      if diff > 0 then
        add_nlst l1 (l2 @ [0])
      else
        add_nlst (l1 @ [0]) l2
  in
  let lst_of_int num =
    let rec aux n acc =
      if n = 0 then
        acc
      else
        aux (n / 10) ((n mod 10) :: acc)
    in
    List.rev (aux num [])
  in
  let ndigits n_lst = List.length n_lst in
  let rec loop cnt bn cn result =
    if cnt = 0 then
      result
    else (
      if ndigits bn > ndigits cn then
        loop (pred cnt) (add_nlst cn cn |> add_nlst bn) (add_nlst cn bn) (succ result)
      else
        loop (pred cnt) (add_nlst cn cn |> add_nlst bn) (add_nlst cn bn) result
    )
  in
  loop num (lst_of_int 3) (lst_of_int 2) 0

let () =
  Printf.printf "Answer:\n";
  Printf.printf "   %d: (w/ Zarith module)\n" (solve_1 1_000);
  Printf.printf "   %d: (w/o Zarith module)\n" (solve_2 1_000)
