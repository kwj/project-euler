(* Project Euler: Problem 53 *)

(*
 *  >>> import math
 *  >>> math.log10(math.factorial(100))
 *  157.97000365471578
 *  >>> math.log10(math.factorial(50))
 *  64.48307487247203
 *
 *  Since the above result, I need to use arbitrary-precision arithmetic library or another method.
 *  Therefore, I use Pascal's triangle formula.
 *  C(n,r) = C(n-1,r-1) + C(n-1,r)
 *  C(n,0) = C(n,n) = 1
 *
 *  C(2n, n) = C(2(n-1), n-1) * (3 + (n-2)/n)
 *           = C(2(n-1), n-1) * (4n-2)/n
 *)

open Core

let compute max_n thr =
  assert (max_n > 0);

  (* find the first central binomial coefficient C(2k, k) which is larger than 'thr'. *)
  let c = ref 2
  and x = ref 1 in
  while !c <= thr do
    x := !x + 1;
    c := !c * ((4 * !x) - 2) / !x
  done;

  (* the start position 'c' = C(n, r) is C(2k-1, k-1). And then set x = (n - r + 1). *)
  let r = ref (!x - 1) in
  x := !x + 1;
  c := !c / 2;

  let rec loop ans = function
    | [] -> ans
    | n :: ns ->
      while !c > thr do
        c := !c * !r / !x;
        r := !r - 1;
        x := !x + 1
      done;

      (* go to next row (down-left) *)
      c := !c * (n + 1) / !x;
      x := !x + 1;
      loop (ans + n - (2 * !r) - 1) ns
  in
  loop 0 (List.range (!r + !x - 1) max_n ~stop:`inclusive)
;;

let solve () = compute 100 1_000_000 |> Int.to_string

(* Test *)

let%test_unit "23, 1_000_000" = [%test_eq: int] (compute 23 1_000_000) 4
let%test_unit "100, 1_000_000" = [%test_eq: int] (compute 100 1_000_000) 4075
