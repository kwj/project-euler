(* Project Euler: Problem 62 *)

(* ---------------------------------------------------------------- *)

let compute num_of_perm =
  let tbl = Hashtbl.create 1024 in
  let make_key num =
    List.fold_left (^) "" (List.sort compare (Str.split (Str.regexp "") (string_of_int num)))
  in
  let rec loop n result =
    let cube = n * n * n in
    let key = make_key cube in
    if String.length key > List.nth result 1 then
      result
    else (
      (match Hashtbl.find_opt tbl key with
       | None -> Hashtbl.add tbl key [cube]
       | Some v -> Hashtbl.replace tbl key (cube :: v));
      if List.length (Hashtbl.find tbl key) < num_of_perm then
        loop (succ n) result
      else
        let tmp = List.hd (List.rev (Hashtbl.find tbl key)) in
        if tmp < List.hd result then
          loop (succ n) [tmp; String.length key]
        else
          loop (succ n) result
    )
  in
  loop 1 [max_int; max_int]      (* [cube; num_of_digits(cube)] *)

let solve () = compute 5

let () =
  Printf.printf "Answer: %d\n" (List.hd (solve()))
