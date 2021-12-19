(* Project Euler: Problem 95 *)

(*
  This program is slow.

  The 'divsum' table is made in a naively honest way, and it will be
  faster if we improve this. We can modify sieve of Eratosthenes to
  create it directly, instead of creating it from divisors.
 *)

(* ---------------------------------------------------------------- *)

class eratosthenes limit =
  object(self)
    val prime_tbl = Array.make (limit + 1) true
    val minfactor_tbl = Array.make (limit + 1) (-1)

    method is_prime n =
      prime_tbl.(n)

    method factorize n =
      let rec loop n result =
        if n < 2 then
          List.rev result
        else
          let p = minfactor_tbl.(n) in
          let rec loop' n e =
            if minfactor_tbl.(n) <> p then
              n, (p, e)
            else
              loop' (n / p) (succ e)
          in
          let new_n, elm = loop' n 0 in
          loop new_n (elm :: result)
      in
      loop n []

    method divisors n =
      let mul_lst i p e =
        let rec loop e acc =
          if e = 0 then
            List.rev acc
          else
            loop (pred e) (((List.hd acc) * p) :: acc)
        in
        loop e [i]
      in
      let rec loop pf_lst result =
        match pf_lst with
        | [] -> List.sort compare result
        | (prime, exp) :: tl ->
           loop tl (List.map (fun i -> mul_lst i prime exp) result |> List.flatten)
      in
      if n <= 0 then
        raise (Failure "n <= 0")
      else
        loop (self#factorize n) [1]
      
    initializer
      begin
        prime_tbl.(1) <- false;
        minfactor_tbl.(1) <- 1;
        for i = 2 to limit do
          if prime_tbl.(i) = false then ()
          else (
            let rec loop j =
              if j > limit then ()
              else (
                prime_tbl.(j) <- false;
                if minfactor_tbl.(j) <> -1 then ()
                else (
                  minfactor_tbl.(j) <- i
                );
                loop (j + i)
              )
            in
            minfactor_tbl.(i) <- i;
            loop (i * 2)
          )
        done
      end
  end

let make_divsum er num =
  let tbl = Array.make (num + 1) 1 in
  for i = 2 to num do
    tbl.(i) <- (List.fold_left (+) 0 (er#divisors i)) - i
  done;
  tbl

let rec trim_lst n lst =
  match lst with
  | [] -> []
  | hd :: tl when hd = n -> lst
  | hd :: tl -> trim_lst n tl

let solve limit =
  let er = new eratosthenes limit in
  let divsum_tbl = make_divsum er limit in
  let visited_tbl = Array.make (limit + 1) 0 in
  let rec loop num result =
    if num < 1 then
      result
    else (
      let rec chain_loop n lst =
        if n > limit || n = 1 then
          None
        else
          if visited_tbl.(n) <> 0 then
            if lst = [] then None else Some (n, lst)
          else (
            visited_tbl.(n) <- 1;
            chain_loop divsum_tbl.(n) (n :: lst)
          )
      in
      match chain_loop num [] with
      | None -> loop (pred num) result
      | Some (n, lst) ->
         if List.hd lst = n then
           loop (pred num) result
         else
           let chain_lst = trim_lst n (List.rev lst) in
           if List.length chain_lst > List.length result then
             loop (pred num) chain_lst
           else
             loop (pred num) result
    )
  in
  loop limit []

let () =
  let l = solve 1_000_000 in
  Printf.printf "Answer: %d [length of chain=%d]\n" (List.hd (List.sort compare l)) (List.length l)
