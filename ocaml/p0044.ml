(* Project Euler: Problem 44 *)

(* ---------------------------------------------------------------- *)

(*
  P(n) = n * (3n - 1) / 2 = k
    -> 3n^2 - n = 2k
    -> 3n^2 - 2 - 2k = 0
    -> n = (1 + sqrt(1 + 24k)) / 6   [because n > 0]
      if 'k' is pentagonal, '(1 + sqrt(1 + 24k)) / 6' must be a natural number.

  let D(k,j) as the following conditions are satisfied:
    * P(k) - P(j) and P(k) + P(j) are both pentagonal
    * D(k,j) = abs(P(k) - P(j))
    * 0<j<k
 *)

let is_pentagonal num =
  let tmp = sqrt (float_of_int (1 + 24 * num)) in
  if tmp = floor tmp && (int_of_float tmp) mod 6 = 5 then true else false

let penta n = n * (3 * n - 1) / 2


(* -- method 1 -- *)
(*
  step #1:
    starting from k=2, find D(k,j)
  step #2
    check D(k,j) is minimal until P(m) - P(m-1) > D(k,j).
    if there is smaller D(y,x) than D(k,j), continue by
    using D(y,x), intead of D(k,j).
 *)

let find_min_d_1 lst =
  let rec aux num plst d result =
    if 3 * (succ num) - 2 > d then      (* until P(n) - P(n-1) = 3n - 2 > D *)
      result
    else (
      let next_p = (3 * num + 1) + (List.hd plst) in      (* P(n+1) = (3n + 1) + P(n) *)
      let next_plst = List.filter (fun e -> next_p - e <= d) plst in      (* pruning *)
      let diff_lst = List.filter (is_pentagonal) (List.map ((-) next_p) next_plst) in
      if diff_lst = [] then
        aux (succ num) (next_p :: next_plst) d result
      else
        let sum_lst = List.filter (is_pentagonal) (List.map ((-) (2 * next_p)) diff_lst) in
        if sum_lst = [] then
          aux (succ num) (next_p :: next_plst) d result
        else
          let tmp = List.hd sum_lst - next_p in
          if next_p - tmp < d then
            aux (succ num) (next_p :: next_plst) (next_p - tmp) ((succ num), next_p, tmp)
          else
            aux (succ num) (next_p :: next_plst) d result
    )
  in
  aux (List.length lst) lst max_int (0,0,0)


(* -- method 2 -- *)
(*
  P(n) = n(3n-1) / 2
  let P(d) = D(k,j) = P(k) - P(j) [j<k]
    let x = k - j, [0<x<d]]
      P(d) = P(k) - P(k-x)
           = (k(3k-1) - (k-x)(3(k-x)-1)) / 2
           = (3k^2 - k - (k-x)(3k-3x-1)) / 2
           = (3k^2 - k - (3k^2-3kx-k-3kx+3x^2+x)) / 2
           = (3k^2 - k - (3k^2-6kx+3x^2-k+x)) / 2
           = (3k^2 - k - 3k^2 + 6kx + 3x^2 + k - x) / 2
           = (6kx - 3x^2 - x) / 2
           = 3kx - x(3x+1)/2
  -->  3kx = P(d) + x(3x+1)/2
  -->    k = (P(d) + x(3x+1)/2) / 3x  [0<x<d]
          if k is existed as a natural number, a pair of P(k) and P(j=k-x) is exist
          and (P(d) + x(3x+1)/2) mod 3 = 0
          --> ((d(3d-1) + x(3x+1)) / 2) mod 3 = 0
              (x - d) mod 3 = 0  [0<x<d]
 *)

let find_min_d_2 num =
  let rec find_kj pd x result =
    if x <= 0 then
      result
    else
      if (pd + x * (3 * x + 1) / 2) mod (3 * x) = 0 then
        let k = (pd + x * (3 * x + 1) / 2) / (3 * x) in
        if is_pentagonal ((penta k) + (penta (k - x))) then
          find_kj pd (x - 3) ((k, k - x) :: result)
        else
          find_kj pd (x - 3) result
      else
        find_kj pd (x - 3) result
  in
  let rec aux d =
    match find_kj (penta d) (d - 3) [] with
    | [] -> aux (succ d)
    | lst -> lst
  in
  aux num


(* -- method 3 -- *)
(*
  P(n) = n(3n-1) / 2
  let P(d) = D(k,j) = P(k) - P(j) [j<k]
    let x = k - j, [0<x<d]]
      P(d) = P(j+x) - P(j)
           = ((j+x)(3(j+x)-1) - j(3j-1)) / 2
           = ((j+x)(3j+3x-1) - j(3j-1)) / 2
           = ((3j^2 + 3jx - j + 3jx + 3x^2 - x) - (3j^2 - j)) / 2
           = (3j^2 + 3jx - j + 3jx + 3x^2 - x - 3j^2 + j) / 2
           = (6jx + 3x^2 -x) / 2
           = 3jx + x(3x - 1) / 2
           = 3jx + P(x)
  -->  3jx = P(d) - P(x)
  -->    j = (P(d) - P(x)) / 3x [0<x<d]
          if j is existed as a natural number, a pair of P(k=j+x) and P(j) is exist.
          and (P(d) - P(x) mod 3 = 0
          --> ((d(3d-1) - x(3x-1)) / 2) mod 3 = 0
              (x - d) mod 3 = 0  [0<x<d]
 *)

let find_min_d_3 num =
  let rec find_kj pd x result =
    if x <= 0 then
      result
    else
      let px = penta x in
      if (pd - px) mod (3 * x) = 0 then
        let j = (pd - px) / (3 * x) in
        if is_pentagonal ((penta (j + x)) + (penta (j))) then
          find_kj pd (x - 3) ((j + x, j) :: result)
        else
          find_kj pd (x - 3) result
      else
        find_kj pd (x - 3) result
  in
  let rec aux d =
    match find_kj (penta d) (d - 3) [] with
    | [] -> aux (succ d)
    | lst -> lst
  in
  aux num


let () =
  let k, pk, pj = find_min_d_1 [1] in
  Printf.printf "[Method 1]\nAnswer: %d  [P(k)=%d, P(j)=%d, (k=%d,j=%d)]\n"
    (pk - pj) pk pj k ((1 + int_of_float (sqrt (float_of_int (1 + 24 * pj)))) / 6);

  List.iter (fun (k, j) ->
      let pk = penta k in
      let pj = penta j in
      Printf.printf "[Method 2]\nAnswer: %d  [P(k)=%d, P(j)=%d, (k=%d,j=%d)]\n" (pk - pj) pk pj k j)
    (find_min_d_2 1);

  List.iter (fun (k, j) ->
      let pk = penta k in
      let pj = penta j in
      Printf.printf "[Method 3]\nAnswer: %d  [P(k)=%d, P(j)=%d, (k=%d,j=%d)]\n" (pk - pj) pk pj k j)
    (find_min_d_3 1)
