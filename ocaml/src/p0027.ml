(* Project Euler: Problem 27 *)

(*
   when 'n' = 0:
   'b' must be a prime number. so, 1 < 'b' < 1000.
   when 'n' = 1:
   '1 + a + b' must be a prime number. Assume this prime number is 'x', 'a' = 'x' - 'b' - 1.
   Therefore, abs('x' - b - 1) < 1000 and 1 < 'b' < 1000, hence 0 < 'x' < 2000.
   when 'n' is odd number:
   'n^2 + b' is a even number. so 'a' must be a odd number.
   the value of 'b' cannot be 2:
   bacause value of the expression becomes even when 'n' is an even number. it's not a prime number.
*)

open Core
module Prime = Euler.Math.Prime

let count_consecutive a b =
  let rec loop n = if Prime.is_prime ((n * n) + (a * n) + b) then loop (succ n) else n in
  loop 0
;;

let compute () =
  let primes = Prime.primes 1 2000 in
  let b_cands = List.(primes |> tl_exn |> filter ~f:(fun n -> n < 1000)) in
  let rec aux max_len max_tpl = function
    | [] -> fst max_tpl * snd max_tpl
    | b :: bs ->
      let opt_tpl =
        List.(
          primes
          |> map ~f:(fun x -> (x - b - 1, b)) (* (a, b) *)
          |> filter_map ~f:(fun ((a, b) as tpl) ->
            if abs a < 1000 then Some (count_consecutive a b, tpl) else None)
          |> max_elt ~compare:(fun (x, _) (y, _) -> Int.compare x y))
      in
      (match opt_tpl with
       | Some (len, tpl) ->
         if len > max_len then aux len tpl bs else aux max_len max_tpl bs
       | None -> failwith "no answer")
  in
  aux 0 (0, 0) b_cands
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "longest" = [%test_eq: int] (compute ()) (-59231)
