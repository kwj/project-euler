(* Project Euler: Problem 72 *)

(*
  numbers of reduced proper fraction when the denominator is 'n'
  is equal to phi(n).  (n > 1)

  the answer is  sum(phi(n))  [n=2, 3, ..., 1_000_000]
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

let solve num =
  let phi_tbl = make_phi_tbl num in
  Array.fold_left (+) 0 (Array.sub phi_tbl 2 (Array.length phi_tbl - 2))

let () =
  Printf.printf "Answer: %d\n" (solve 1_000_000)
