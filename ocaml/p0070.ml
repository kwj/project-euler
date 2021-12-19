(* Project Euler: Problem 70 *)

(*
  the answer 'n' can't be a prime number. Becase phi(n) = n-1 when n is a prime number.
  'n' is not a permutation of 'n-1'.

  so the answer must be a composite number.

  this program is slow.
 *)

(* ---------------------------------------------------------------- *)

let make_phi_tbl limit =
  let phi_tbl = Array.init (limit + 1) (fun i -> i) in
  for i = 2 to limit do
    if phi_tbl.(i) = i then
      let rec aux j =
        if j <= limit then (
          phi_tbl.(j) <- phi_tbl.(j) - (phi_tbl.(j) / i);
          aux (j + i)
        )
      in
      aux i
  done;
  phi_tbl

let is_perm_num n1 n2 =
  let sort_digits n = List.fold_left (^) "" (List.sort compare (Str.split (Str.regexp "") (string_of_int n))) in
  if sort_digits n1 = sort_digits n2 then true else false

let solve limit =
  let phi_tbl = make_phi_tbl limit in
  let rec loop i result =
    if i > limit then
      result
    else
      if phi_tbl.(i) = (i - 1) then      (* skip prime number *)
        loop (succ i) result
      else
        let current_min, _ = result in
        let ratio = (float_of_int i) /. (float_of_int phi_tbl.(i)) in
        if ratio >= current_min then
          loop (succ i) result
        else
          if is_perm_num i phi_tbl.(i) = false then
            loop (succ i) result
          else
            loop (succ i) (ratio, (i, phi_tbl.(i)))
  in
  loop 4 (max_float, (0, 0))    (* n=2,3 are prime numbers *)

let () =
  let ratio, (n, phi_n) = solve (10_000_000 - 1) in
  Printf.printf "Answer: n=%d phi(%d)=%d  n/phi(n)=%f\n" n n phi_n ratio
