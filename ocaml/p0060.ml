(* Project Euler: Problem 60 *)

(*
  This program is a bit slow. Here is a result on Raspberry Pi 4 Model B.

    % time ./p0060
    Answer: 26033
    ./p0060  8.74s user 0.11s system 99% cpu 8.885 total
 *)

(* ---------------------------------------------------------------- *)

module Prime : sig
  type t
  val init : unit -> t
  val is_prime : int -> bool
  val current : t -> int
  val next : t -> int
  val nth : t -> int -> int
end = struct
  type t = { mutable pool : int list; }
  let init () = { pool = [] }

  (*
    Miller–Rabin primality test (deterministic version under 2^64)
      [Miller–Rabin primality test](https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test)
      [Deterministic variants of the Miller-Rabin primality test](http://miller-rabin.appspot.com/)
   *)
  type typenum = Composite | Prime | Undecided
  let is_prime num =
    if num <= 2 then num = 2
    else
      if num mod 2 = 0 then false
      else
        let init_value num =
          let rec aux s d =
            if d mod 2 <> 0 then s, d else aux (succ s) (d / 2)
          in
          aux 0 (pred num)
        in
        let s, d = init_value num in
        let is_prime' a =
          let x = Z.(to_int (powm ~$a ~$d ~$num)) in
          if x = 0 then Prime
          else
            if x = 1 || x = num - 1 then Undecided
            else
              let rec loop x' cnt =
                if cnt = 0 then Composite
                else
                  let x'' = Z.(to_int ((~$x' * ~$x') mod ~$num)) in
                  if x'' = 0 then Prime
                  else
                    if x'' = num - 1 then Undecided else loop x'' (pred cnt)
              in
              loop x (pred s)
        in
        let rec sprp_loop = function
          | [] ->
             true
          | hd :: tl ->
             match is_prime' hd with
             | Prime -> true
             | Composite -> false
             | Undecided -> sprp_loop tl
        in
        sprp_loop [2; 325; 9375; 28178; 450775; 9780504; 1795265022]      (* Jim Sinclair's bases *)

  let rec next_prime i =
    if i = 2 then 3
    else
      let cand = i + 2 in
      if is_prime cand then cand else next_prime cand

  let current p =
    if List.length p.pool = 0 then (
      p.pool <- 2 :: p.pool;
      List.hd p.pool
    ) else
      List.hd p.pool

  let next p =
    if List.length p.pool = 0 then (
      p.pool <- 2 :: p.pool;
      current p
    ) else (
      p.pool <- next_prime (current p) :: p.pool;
      current p
    )
    
  let rec nth p i =
    if List.length p.pool < i then (
      let _ = next p in
      nth p i
    ) else (
      List.nth p.pool (List.length p.pool - i)
    )
end        
    
(* find prime numbers that are pair with prime number 'v' from vtbl *)
(* return the list of numbers in descending order *)
let find_nbrs v tbl =
  let cat_num n1 n2 = int_of_string (string_of_int n1 ^ string_of_int n2) in
  Hashtbl.to_seq_keys tbl
  |> Seq.filter (fun n -> Prime.is_prime (cat_num v n) && Prime.is_prime (cat_num n v))
  |> List.of_seq
  |> List.sort (fun e1 e2 -> e2 - e1)

(* return all k-combinations from list *)
let rec comb r lst =
  match r, lst with
  | 0, _ -> [[]]
  | _, []  -> []
  | n, hd :: tl -> List.map (List.cons hd) (comb (n - 1) tl) @ comb n tl

(* check to see if all prime numbers in list are in pairs *)
let check_clique v_lst tbl =
  let rec check_clique' = function
    | [] -> true
    | hd :: tl ->
       let rec is_subset elms super_set =
         match elms with
         | [] -> true
         | e :: es ->
            if List.mem e super_set then is_subset es super_set else false
       in
       if is_subset tl (Hashtbl.find tbl hd) then check_clique' tl else false
  in
  check_clique' v_lst

(*
  paramter clique: [sum of p1..p5; p1; p2; p3; p4; p5]
    and p1 > p2, p3, p4, p5
*)
let find_limit clique tbl =
  let sum_value, crnt_prime  = List.nth clique 0, List.nth clique 1 in
  let cands = Hashtbl.to_seq tbl
              |> Seq.filter (fun (e, _) -> e <> crnt_prime)
              |> List.of_seq
              |> List.map (fun (e, lst) ->
                     if List.length lst < 4 then
                       List.filter (fun l -> check_clique l tbl) [e :: lst]
                     else
                       List.filter (fun l -> check_clique l tbl) (List.map (fun l -> e :: l) @@ comb 3 lst))
            
              |> List.flatten
              |> List.filter ((<>) []) in
  let choose_limit size lst =
    let smallest_sum n =
      let llst = List.filter (fun l -> List.length l = n) lst in
      if List.length llst = 0 then
        None
      else
        (* Using List.map resulted in stack overflow. *)
        Some (List.fold_left (fun min lst -> let tmp = List.fold_left (+) 0 lst in
                                             if tmp < min then tmp else min) max_int llst)
    in
    let rec aux i result =
      if i = 0 then
        result
      else
        match smallest_sum i with
        | None -> aux (pred i) result
        | Some v ->
           let tmp = sum_value - v in
           if tmp >= crnt_prime * (size - i) then
             aux (pred i) (tmp / (size - i))
           else
             aux (pred i) result
    in
    aux (pred size) crnt_prime
  in
  choose_limit 5 cands

let compute prime_gen vtbl =
  let rec loop limit result =
    let next_p = Prime.next prime_gen in
    if next_p > limit then
      result
    else
      let nbrs = find_nbrs next_p vtbl in
      Hashtbl.add vtbl next_p nbrs;
      if List.length nbrs < 4 then
        loop limit result
      else
        let rec aux tgts acc =
          match tgts with
          | [] -> acc
          | hd :: tl ->
             if check_clique hd vtbl then
               let sum = next_p + (List.fold_left (+) 0 hd) in
               if sum < List.hd result then aux tl (sum :: (next_p :: hd)) else aux tl acc
             else
               aux tl acc
        in
        let clique = aux (comb 4 nbrs) result in
        if List.hd clique < List.hd result then
          loop (find_limit clique vtbl) clique
        else
          loop limit result
  in
  loop max_int [max_int]

(*
  vtbl: prime pair hash table
    key - prime number
    value - list of prime numbers less than 'key' taht is a pair with 'key'
 *)
let solve () =
  let prime_gen = Prime.init() in
  let vtbl = Hashtbl.create 4096 in
  let _ = Prime.nth prime_gen 3 in    (* drop '2', '3' and '5' *)
  Hashtbl.add vtbl 3 [];
  List.hd (compute prime_gen vtbl)
  
let () =
  Printf.printf "Answer: %d\n" (solve())
