(* Project Euler: Problem 74 *)

(*
  case 1:
    [a; b; c; KEY; e; f] when fact_sum(f) = KEY
      --> lst: [a; b; c; KEY; e; f]
          key: KEY
          offset: 0
  case 2:
    [g; h; i; j] when fact_sum(j) is already exist in memo_tbl 
      --> lst: [g; h; i; j]
          key: 0
          offset: Hashtbl.find memo_tbl fact_sum(j)

  [example of memo_tbl]
    source:
      69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)
    result:
      (69, 5), (363600, 4), (1454, 3), (169, 3), (363601, 3)
 *)

(* ---------------------------------------------------------------- *)

let fact_sum num =
  let rec factorial n =
    if n <= 0 then 1 else n * factorial (n - 1)
  in
  let rec loop n result =
    if n = 0 then
      result
    else
      loop (n / 10) (result + factorial (n mod 10))
  in
  loop num 0

let add_memo tbl lst key offset =
  let rec loop l slen =
    match l with
    | [] -> ()
    | hd :: tl ->
       if slen <> 0 then (
         Hashtbl.add tbl hd slen;
         loop tl slen
       ) else (
         Hashtbl.add tbl hd ((List.length l) + offset);
         if hd = key then
           loop tl (List.length l)
         else
           loop tl slen
       )
  in
  loop lst 0

let solve limit =
  let memo_tbl = Hashtbl.create 4096 in
  let rec loop' num lst =
    let next = fact_sum num in
    match Hashtbl.find_opt memo_tbl next with
    | None ->
       if List.mem next lst then
         (List.rev lst), next, 0
       else
         loop' next (next :: lst)
    | Some v ->
       (List.rev lst), 0, v
  in
  let rec loop num result =
    if num < 1 then
      result
    else (
      let lst, key, offset = loop' num [num] in
      add_memo memo_tbl lst key offset;
      if (List.length lst) + offset = 60 then
        loop (pred num) (num :: result)
      else
        loop (pred num) result
    )
  in
  List.length (loop limit [])

let () =
  Printf.printf "Answer: %d\n" (solve 999_999)
