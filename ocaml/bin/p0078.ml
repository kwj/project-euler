(* Project Euler: Problem 78 *)

(*
  p(5) = 7
  p(10) = 42
  p(50) = 204226
  p(100) = 190569292
  p(200) = 3972999029388
  p(500) = 2300165032574323995027
  p(1000) = 24061467864032622473692149727991
    ...

  Hmm, I needed to find another way instead of dynamic programming.
  Finally, I gave up trying to solve it on my own and I saw following pages.

    https://en.wikipedia.org/wiki/Partition_(number_theory)
    https://en.wikipedia.org/wiki/Partition_function_(number_theory)
    https://en.wikipedia.org/wiki/Pentagonal_number_theorem
*)

(* ---------------------------------------------------------------- *)

let penta_memo = Hashtbl.create 4096

let penta_num i =
  match Hashtbl.find_opt penta_memo i with
  | Some v -> v
  | None ->
     let j = (i + 1) / 2 in
     let v =
       match i mod 2 with
       | 1 -> j * (3 * j - 1) / 2
       | _ -> ((-1) * j) * (3 * ((-1) * j) - 1) / 2 in
     Hashtbl.add penta_memo i v;
     v

let right_side p_tbl n =
  let sign_tbl = [|1; 1; -1; -1|] in
  let rec loop i result =
    let j = penta_num i in
    if j > n then
      result
    else
      loop (succ i) (result + sign_tbl.((i - 1) mod 4) * (Hashtbl.find p_tbl (n - j)))
  in
  loop 1 0

let solve num =
  let p_tbl = Hashtbl.create 4096 in
  Hashtbl.add p_tbl 0 1;
  let rec loop n =
    let next_p = (right_side p_tbl n) mod num in
    if next_p = 0 then
      n
    else (
      Hashtbl.add p_tbl n next_p;
      loop (succ n)
    )
  in
  loop 1

let () =
  Printf.printf "Answer: %d\n" (solve 1_000_000)
