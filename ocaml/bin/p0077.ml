(* Project Euler: Problem 77 *)

(* ---------------------------------------------------------------- *)

module Prime : sig
  type t
  val init : unit -> t
(*
  val is_prime : int -> bool
  val current : t -> int
  val next : t -> int
  val nth : t -> int -> int
 *)
  val max_prime : t -> int -> int
  val max_prime_lst : t -> int -> int list
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

  let max_prime p n =
    let rec aux n =
      if next p >= n then () else aux n
    in
    aux n;
    List.filter ((>=) n) p.pool |> List.hd

  let max_prime_lst p n =
    let rec aux n =
      if next p >= n then () else aux n
    in
    aux n;
    List.filter ((>=) n) p.pool |> List.rev
end        

let solve_dp ways =
  let p_gen = Prime.init() in
  let rec loop n =
    let tbl = Array.make (n + 1) 0 in
    let plst = Prime.max_prime_lst p_gen n in
    let rec loop' lst =
      match lst with
      | [] -> tbl.(n)
      | hd :: tl ->
         for i = hd to n do
           tbl.(i) <- tbl.(i) + tbl.(i - hd)
         done;
         loop' tl
    in
    tbl.(0) <- 1;
    if loop' plst >= ways then
      n
    else
      loop (succ n)
  in
  loop 2

let () =
  Printf.printf "Answer: %d\n" (solve_dp 5_000)
