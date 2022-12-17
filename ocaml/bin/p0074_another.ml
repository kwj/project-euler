(* Project Euler: Problem 74 *)

(* Loopback supported *)

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

open Core

let fact_sum num =
  let fact = [|1; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880|] in
  let rec loop n result =
    if n = 0 then
      result
    else
      loop (n / 10) (result + fact.(n mod 10))
  in
  loop num 0

let add_memo tbl lst key offset =
  let rec aux l len =
    match l with
      [] -> ()
    | x :: xs ->
        if len <> 0 then (
          Hashtbl.set tbl ~key:x ~data:len;
          aux xs len
        ) else (
          match Hashtbl.add tbl ~key:x ~data:((List.length l) + offset) with
          | `Ok -> if x = key then aux xs (List.length l) else aux xs len
          | `Duplicate -> ()
        )
  in
  aux lst 0

let solve limit =
  let memo_tbl = Hashtbl.create (module Int) in
  let rec aux num lst =
    let next_num = fact_sum num in
    match Hashtbl.find memo_tbl next_num with
    | None ->
        if List.mem lst next_num ~equal then
          (List.rev lst), next_num, 0
        else
          aux next_num (next_num :: lst)
    | Some v ->
        (List.rev lst), 0, v
  in
  let rec loop num result =
    if num < 1 then
      result
    else (
      let lst, key, offset = aux num [num] in
      add_memo memo_tbl lst key offset;
      if (List.length lst) + offset = 60 then
        loop (pred num) (succ result)
      else
        loop (pred num) result
    )
  in
  loop limit 0

let exec () =
  Int.to_string (solve 999_999)

let () = Euler.Task.run exec
