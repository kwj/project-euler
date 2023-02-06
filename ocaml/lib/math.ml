
let rec gcd m n =
  (* Euclidean algorithm *)
  if n = 0 then abs m else gcd n (m mod n)

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)

let num_of_digits num =
  truncate (log10 (float num)) + 1

let is_permutation x y =
  let tbl = Array.init 10 (fun _ -> 0) in
  let count fn n =
    let rec aux i =
      if i <> 0 then (
	tbl.(i mod 10) <- (fn tbl.(i mod 10) 1);
	aux (i / 10)
      ) else (
	()
      )
    in
    aux	n
  in
  count (+) x;
  count (-) y;
  Array.for_all	(fun i -> i = 0) tbl

let bit_length num =
  if num = 0 then
    0
  else
    let rec aux n cnt =
      if n = 0 then cnt else aux (n lsr 1) (succ cnt)
    in
    aux num 0

let bin_of_int num =
  let rec conv = function
    | (0, binary) -> binary
    | (decimal, binary) -> conv (decimal / 2, (string_of_int @@ decimal mod 2) ^ binary)
  in
  match num with
    | 0 -> "0"
    | n when n > 0 -> conv (n, "")
    | n -> let bin = conv (n + max_int + 1, "") in
           "1" ^ (Printf.sprintf "%62s" bin |> String.map (fun c -> if c = ' ' then '0' else c))

(*
  Integer Square Root

  https://github.com/mdickinson/snippets/blob/master/proofs/isqrt/src/isqrt.lean

  def isqrt_aux(c, n):
      if c == 0:
          return 1
      else:
          k = (c - 1) // 2
          a = isqrt_aux(c // 2, n >> 2*k + 2)
          return (a << k) + (n >> k+2) // a

  def isqrt(n):
      if n == 0:
          return 0
      else:
          a = isqrt_aux((n.bit_length() - 1) // 2, n)
          return a - 1 if n < a * a else a
*)
let isqrt num =
  let rec aux c n =
    if c = 0 then
      1
    else
      let k = (c - 1) / 2 in
      let a = aux (c / 2) (n lsr (2 * k + 2)) in
      (a lsl k) + ((n lsr (k + 2)) / a)
  in
  if num = 0 then
    0
  else
    let a = aux (((bit_length num) - 1) / 2) num in
    if num < a * a then
      a - 1
    else
      a

let is_triangular num =
  let tmp = 8 * num + 1 in
  let tmp_sqrt = isqrt tmp in
  tmp = tmp_sqrt * tmp_sqrt && tmp_sqrt mod 2 = 1

let is_square num =
  let num_sqrt = isqrt num in
  num = num_sqrt * num_sqrt

let is_pentagonal num =
  let tmp = 24 * num + 1 in
  let tmp_sqrt = isqrt tmp in
  tmp = tmp_sqrt * tmp_sqrt && tmp_sqrt mod 6 = 5

let is_hexagonal num =
  let tmp = 8 * num + 1 in
  let tmp_sqrt = isqrt tmp in
  tmp = tmp_sqrt * tmp_sqrt && tmp_sqrt mod 4 = 3

let divisors n =
  let u_limit = isqrt n in
  let rec aux lst i =
    if i > u_limit then
      lst
    else
      if n mod i <> 0 then
        aux lst (succ i)
      else
        if i * i = n then
          aux (i::lst) (succ i)
        else
          aux ((n / i) :: (i::lst)) (succ i)
  in
  List.sort compare (aux [] 1)

(* 126 -> [(2, 1); (3, 2); (7, 1)] *)
let factorize num =
  let div_loop m n pfactor_lst =
    let rec aux m n acc =
      if m mod n <> 0 then
        let next_n = if n mod 2 = 0 then 3 else n + 2 in
        if acc <> 0 then
          m, next_n, (n, acc) :: pfactor_lst
        else
          m, next_n, pfactor_lst
      else
        aux (m / n) n (succ acc)
    in
    aux m n 0
  in
  let rec loop m n acc =
    if m = 1 then
      List.rev acc
    else
      let next_m, next_n, new_acc = div_loop m n acc in
      loop next_m next_n new_acc
  in
  loop num 2 []

let pfactors_to_divisors pf_lst =
  let rec pow b e =
    match e with
    | 0 -> 1
    | 1 -> b
    | n -> let x = pow b (e / 2) in
           x * x * (if n mod 2 = 0 then 1 else b)
  in
  let rec aux divs (base, exp) acc =
    if exp = 0 then
      acc
    else
      aux divs (base, (pred exp)) (List.map (fun n -> n * (pow base exp)) divs @ acc)
  in
  let rec loop_factor lst result =
    match lst with
    | (b, e) :: xs -> loop_factor xs (aux result (b, e) result)
    | [] -> result
  in
  loop_factor pf_lst [1] |> List.sort compare

let is_pandigital num =
  let mk_bits num =
    let rec aux n bits =
      if n = 0 then
        bits
      else
        aux (n / 10) (bits lor (1 lsl (n mod 10)))
    in
    aux num 0
  in
  mk_bits num = (1 lsl (num_of_digits num) - 1)

let is_pandigital_nz num =
  let rec check_zero n =
    if n = 0 then
      true
    else
      if n mod 10 = 0 then
        false
      else
        check_zero (n / 10)
  in
  num > 0 && check_zero num && is_pandigital (num * 10)

let is_pandigital_str_nz s =
  is_pandigital_nz (int_of_string s)

let is_pandigital_strlst_nz lst =
  is_pandigital_str_nz (List.fold_left (fun acc s -> acc ^ s) "" lst)

let is_pandigital_lst_nz lst =
  is_pandigital_strlst_nz (List.map (string_of_int) lst)

let is_palindrome ?(base=10) num =
  let rec loop n acc =
    if n = 0 then
      acc
    else
      loop (n / base) (acc * base + (n mod base))
  in
  num = loop num 0

let is_prime num =
  let upper = isqrt num in
  let rec aux n k =
    if k < 2 then
      true
    else
      (n mod k <> 0) && (aux n (k - 1))
  in
  if num <= 1 then false else aux num upper

(*
  Miller–Rabin primality test (deterministic version under 2^64)
  [Miller–Rabin primality test](https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test)
  [Deterministic variants of the Miller-Rabin primality test](http://miller-rabin.appspot.com/)
 *)
type typenum = Composite | Prime | Undecided

let mr_isprime num =
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

module Prime = struct
  type t = {
      size : int;
      prime_tbl : bool array;
      minfactor_tbl : int array;
      mobius_tbl : int array
    }

  (* return: prime table, minimum factor table, mobius function table *)
  let make_tables upper =
    let isprime = Array.init (upper + 1) (fun _ -> true) in
    let minfactor = Array.init (upper + 1) (fun _ -> -1) in
    let mobius = Array.init (upper + 1) (fun _ -> 1) in
    isprime.(0) <- false;
    isprime.(1) <- false;
    minfactor.(1) <- 1;
    for i = 2 to upper do
      if isprime.(i) = true then (
        minfactor.(i) <- i;
        mobius.(i) <- -1;
        let j = ref (i * 2) in
        while !j <= upper do
          isprime.(!j) <- false;
          if minfactor.(!j) = -1 then (
            minfactor.(!j) <- i
          );
          if (!j / i) mod i = 0 then (
            mobius.(!j) <- 0
          ) else (
            mobius.(!j) <- -mobius.(!j)
          );
          j := !j + i
        done
      );
    done;
    { size = upper; prime_tbl = isprime; minfactor_tbl = minfactor; mobius_tbl = mobius }

  let is_prime p num = p.prime_tbl.(num)

  let prime_tbl p = Array.copy p.prime_tbl

  let minfactor_tbl p = Array.copy p.minfactor_tbl

  let mobius_tbl p = Array.copy p.mobius_tbl

  let prime_list p =
    let filter_mapi fn lst =
      let rec loop i l accum =
        match l with
        | [] -> List.rev accum
        | x :: xs ->
           (match fn i x with
            | Some v -> loop (i + 1) xs (v :: accum)
            | None -> loop (i + 1) xs accum)
      in
      loop 0 lst []
    in
    filter_mapi (fun i flag -> if flag = true then Some i else None) (Array.to_list p.prime_tbl)

  let factorize p num =
    let rec aux n result =
      if n = 1 then
        result
      else
        let base = p.minfactor_tbl.(n) in
        let rec div_loop i exp =
          if p.minfactor_tbl.(i) = base then
            div_loop (i / base) (succ exp)
          else
            i, (base, exp)
        in
        let next_n, elt = div_loop n 0 in
        aux next_n (elt :: result)
    in
    if num > p.size then
      assert false
    else
      aux num [] |> List.sort (fun (b1, _) (b2, _) -> compare b1 b2)

  let divisors p num =
    pfactors_to_divisors (factorize p num)

  let generator () =
    let tbl = Hashtbl.create 128 in
    let prime = ref 2 in
    let rec next () =
      match Hashtbl.find_opt tbl !prime with
      | Some lst -> List.iter (fun n -> match Hashtbl.find_opt tbl (n + !prime) with
                                        | None -> Hashtbl.add tbl (n + !prime) [n]
                                        | Some l -> Hashtbl.replace tbl (n + !prime) (n :: l))
                              lst;
                    Hashtbl.remove tbl !prime;
                    prime := !prime + 1;
                    next ()
      | None -> Hashtbl.add tbl (!prime * !prime) [!prime];
                prime := !prime + 1;
                !prime - 1
    in
    next

  let generator2 ?(start=2) () =
    let p = ref (if start mod 2 = 0 && start <> 2 then start - 1 else start - 2) in
    let rec aux n =
      if mr_isprime (n + 2) then (p := n + 2; n + 2) else (aux (n + 2))
    in
    let next () =
      if !p = 2 then (p := 3; 3) else aux !p
    in
    next

end
