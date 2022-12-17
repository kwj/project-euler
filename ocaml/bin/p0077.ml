(* Project Euler: Problem 77 *)

open Core

let plst_generator () =
  let p_gen = Euler.Math.Prime.generator () in
  let p_lst = ref [] in
  let next () =
    p_lst := (p_gen ()) :: !p_lst;
    List.rev !p_lst
  in
  next

let solve ways =
  let plst_gen = plst_generator () in
  let rec loop p_lst =
    let n = List.length p_lst in
    let tbl = Array.create ~len:(n + 1) 0 in
    let rec aux lst =
      match lst with
      | [] -> tbl.(n)
      | x :: xs ->
          for i = x to n do
            tbl.(i) <- tbl.(i) + tbl.(i - x)
          done;
          aux xs
    in
    tbl.(0) <- 1;
    if aux p_lst >= ways then
      n
    else
      loop (plst_gen ())
  in
  loop (plst_gen())
    
let exec () =
  Int.to_string (solve 5_000)

let () = Euler.Task.run exec

