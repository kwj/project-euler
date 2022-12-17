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

open Core

let solve num =
  let search_n m =
    let m2 = m * (m + 1) in
    let rec loop n cand_n diff =
      let prod = m2 * n * (n + 1) in
      let d = abs (num - prod) in
      if prod > num && d > diff then
        cand_n, diff
      else
        if d < diff then
          loop (succ n) n d
        else
          loop (succ n) cand_n diff
    in
    loop (m + 1) 0 Int.max_value
  in
  let module M = Euler.Math in
  let module PQ = Euler.PrioQueue.Make(struct
                      type t = int * (int * int)
                      let compare x y = Int.compare (fst y) (fst x)
                    end) in

  let upper_m = (M.isqrt (M.isqrt num)) + 1 in
  let pq = PQ.init () in
  PQ.insert pq (Int.max_value, (0, 0));
  let rec loop m =
    let n, d = search_n m in
    if m > upper_m && d > fst (PQ.peek pq) then
      PQ.peek pq
    else (
      PQ.insert pq (d, (m, n));
      loop (succ m)
    )
  in
  loop 1

let exec () =
  let _, (m, n) = solve (2_000_000 * 4) in
  sprintf "%d (m=%d, n=%d)" (m * n) m n

let () = Euler.Task.run exec
