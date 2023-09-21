(* Project Euler: Problem 51 *)

(*
   the smallest prime which, by replacing part of the number with same digit,
   is part of an eight prime value family

   -> eight numbers out of '0' to '9' are used for replacement

   1) the last digit is not eligible for replacement
   It make some even numbers after replacement.

   2) the number of digits of the prime numbers is greater than number of digits to be replaced
   the reason is since 1).

   3) the number of digits that can be replaced is only a multiples of 3
   if 'n' is not multiples of 3, some replaced numbers will contain multiples of 3.

   number  digits  sum  'mod 3'    [n>0]
   ---------------------------------------------
   0       n       0    0
   1       n       n    n mod 3
   2       n       2n   2n mod 3
   3       n       3n   3n mod 3 = 0
   4       n       4n   4n mod 3 = n mod 3
   5       n       5n   5n mod 3 = 2n mod 3
   6       n       6n   6n mod 3 = 0
   7       n       7n   7n mod 3 = n mod 3
   8       n       8n   8n mod 3 = 2n mod 3
   9       n       9n   9n mod 3 = 0

   I want to use prime number tables to speed up and simplify. For the
   moment, I assume that prime numbers less than one million. The reason
   is that I want to only consider the case of replacing three digits.

   4) There are at least same three numbers other than last digit.
*)

open Core

let is_family p f_size =
  let p_digits = Euler.Util.digits p in
  let rec loop_figures = function
    | [] -> false
    | n :: ns ->
      let p_arr = List.to_array p_digits in
      let rec loop_masks = function
        | [] -> None
        | mask ::xs ->
          let cnt = ref 1 in
          let rec aux = function
            | [] -> loop_masks xs
            | d :: ds ->
              List.iter mask ~f:(fun idx -> p_arr.(idx) <- d);
              if Euler.Math.Prime.is_prime (Array.to_list p_arr |> Euler.Util.undigits) then cnt := !cnt + 1;
              if !cnt = f_size
              then Some true
              else if (f_size - !cnt) > (9 - d)
              then Some false
              else aux ds
          in
          aux (List.range (n + 1) 9 ~stop:`inclusive)
      in
      (match
        Euler.Util.findall (fun x -> x = n) p_digits
        |> Euler.Util.powerset
        |> List.filter ~f:(fun l -> List.length l mod 3 = 0 && List.length l <> 0 && List.hd_exn l <> 0)
        |> loop_masks
       with
       | None -> loop_figures ns
       | Some res -> res)
  in
  loop_figures (List.range 0 (10 - f_size) ~stop:`inclusive)
;;

let compute f_size =
  Sequence.unfold ~init:3 ~f:(fun n -> Some (n, n + 1))
  |> Sequence.find_map ~f:(fun exp ->
    Euler.Math.Prime.primes (Int.pow 10 exp) (Int.pow 10 (exp + 1))
    |> List.find ~f:(fun p -> is_family p f_size))
  |> Option.value_exn
;;

let solve () = compute 8 |> Int.to_string

(* Test *)

let%test_unit "8" = [%test_eq: int] (compute 8) 121313
