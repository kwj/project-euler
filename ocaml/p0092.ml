(* Project Euler: Problem 92 *)

(*
  9^2 = 81
  -->
        99 -> 9^2 * 2 = 162
       999 -> 9^2 * 3 = 243
      9999 -> 9^2 * 4 = 324
     99999 -> 9^2 * 5 = 405
    999999 -> 9^2 * 6 = 486
   9999999 -> 9^2 * 7 = 567
 *)

(* ---------------------------------------------------------------- *)

let rec sq_digit n acc =
  if n = 0 then
    acc
  else
    let x = n mod 10 in
    sq_digit (n / 10) (acc + (x * x))

let init_tbl tbl =
  let chain num =
    let rec loop n lst =
      if n = 1 || n = 89 then
        n, lst
      else
        loop (sq_digit n 0) (n :: lst)
    in
    loop num []
  in
  tbl.(1) <- 1; tbl.(89) <- 89;
  for i = 2 to 567 do
    if tbl.(i) <> 0 then
      ()
    else
      let result, lst = chain i in
      let rec loop l =
        match l with
        | [] -> ()
        | hd :: tl ->
           tbl.(hd) <- result;
           loop tl
      in
      loop lst
  done;
  tbl

let calc num tbl =
  let chain num =
    let rec loop n lst =
      if tbl.(n) <> 0 then
        tbl.(n), lst
      else
        loop (sq_digit n 0) (n :: lst)
    in
    loop num []
  in
  let result, lst = chain num in
  let rec loop l =
    match l with
    | [] -> ()
    | hd :: tl ->
       (* If we use permutations here, we may be able to reduce the amount of computation. *)
       tbl.(hd) <- result;
       loop tl
  in
  loop lst

let solve limit =
  let result_tbl = init_tbl (Array.make limit 0) in
  let count = ref 0 in
  for i = (limit - 1) downto (243 + 1) do
    calc i result_tbl
  done;
  for i = 1 to limit - 1 do
    if result_tbl.(i) = 89 then
      count := !count + 1
    else
      ()
  done;
  !count

let () =
  Printf.printf "Answer: %d\n" (solve 10_000_000)
