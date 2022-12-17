(* Project Euler: Problem 85 *)

(*
  nCr = n! / ((n-r)! * r!)

      1  2       n-1  n
    +--+--+-- ... --+--+
   1|  |  |   ...   |  |
    +--+--+-- ... --+--+
   2|  |  |   ...   |  |
    +--+--+-- ... --+--+ num of horizontal lines = m + 1
   3|  |  |   ...   |  |
    +--+--+-- ... --+--+
    ....................
    +--+--+-- ... --+--+
   m|  |  |   ...   |  |
    +--+--+-- ... --+--+
      num of vertical lines = n + 1

  (m+1)C2 * (n+1)C2 = m(m+1)/2 * n(n+1)/2 (\approx) 2_000_000
  --> m(m+1)*n(n+1) (\approx) 8_000_000    [assume that m<n]
 *)

(* ---------------------------------------------------------------- *)

let search_n m ref_val =
  let m2 = m * (m + 1) in
  let rec loop n cand_n diff =
    let prod = m2 * n * (n + 1) in
    let d = abs (ref_val - prod) in
    if d > diff && prod > ref_val then
      cand_n, diff
    else
      if d < diff then
        loop (succ n) n d
      else
        loop (succ n) cand_n diff
  in
  loop (m + 1) 0 max_int

let search_m ref_val =
  let border = truncate @@ ceil @@ sqrt @@ ceil @@ sqrt @@ float ref_val in
  let rec loop m cand_m cand_n diff =
    let n, d = search_n m ref_val in
    if d > diff && m > border then
      cand_m, cand_n
    else
      if d < diff then
        loop (succ m) m n d
      else
        loop (succ m) cand_m cand_n diff
  in
  loop 1 0 0 max_int

let solve num = search_m (num * 4)

let () =
  let m, n = solve 2_000_000 in
  Printf.printf "Answer: %d [m=%d, n=%d, mC2*nC2=%d]\n" (m * n) m n (m * (m+1) * n * (n+1) / 4)
