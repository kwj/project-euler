(* Project Euler: Problem 60 *)

(*
  This solution is slow. Here is a result on Raspberry Pi 4 Model B.

  % ./solve.sh 60
  [Problem 60]
  Answer: 26033
  Elapsed time: 27.907s
*)

open Core

let is_pair x y =
  let concat a b =
    let rec aux n = if b > n then aux (n * 10) else n in
    a * (aux 10) + b
  in
  Euler.Math.Prime.mr_isprime (concat x y) && Euler.Math.Prime.mr_isprime (concat y x)
;;

let find_nbrs p p_lst limit =
  List.filter p_lst ~f:(fun n -> (n + p < limit) && (is_pair n p))
;;

let is_clique p_grp tbl =
  let rec is_sublist a b =
    match a, b with
    | [], [] -> true
    | [], _ -> true
    | _, [] -> false
    | (x :: xs), (y :: ys) when x = y -> is_sublist xs ys
    | (_ :: _), (_ :: ys) -> is_sublist a ys
  in
  let rec loop = function
    | [] -> true
    | _ :: [] -> true
    | x :: xs ->
      if is_sublist xs (Hashtbl.find_exn tbl x)
      then loop xs
      else false
  in
  loop p_grp
;;

let compute size_of_clique =
  let prime_lst = [| ref [3]; ref [3] |] in
  let tbl = Hashtbl.create (module Int) in

  let rec loop p ans =
    let p = Euler.Math.Prime.next_prime p in
    if p >= ans
    then ans
    else (
      let idx = (p + 2) mod 3 in
      let nbr_lst = find_nbrs p !(prime_lst.(idx)) ans in
      Hashtbl.set tbl ~key:p ~data:nbr_lst;
      prime_lst.(idx) := p :: !(prime_lst.(idx));

      if List.length nbr_lst < (size_of_clique - 1)
      then loop p ans
      else (
        let rec aux ans = function
          | [] -> ans
          | p_grp :: xs ->
            let tmp = p + (List.sum (module Int) ~f:Fn.id p_grp) in
            if tmp < ans && is_clique p_grp tbl
            then aux (Int.min tmp ans) xs
            else aux ans xs
        in
        loop p (aux ans (Euler.Util.combination (size_of_clique - 1) nbr_lst))
      )
    )
  in
  loop 5 (Int.max_value)
;;

let solve () = compute 5 |> Int.to_string

(* Test *)

let%test_unit "4" = [%test_eq: int] (compute 4) 792
let%test_unit "5" = [%test_eq: int] (compute 5) 26033
