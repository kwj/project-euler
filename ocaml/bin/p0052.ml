(* Project Euler: Problem 52 *)

(* ---------------------------------------------------------------- *)

let cmp_contained_digits n1 n2 =
  let sort_digits n = List.fold_left (^) "" (List.sort compare (Str.split (Str.regexp "") (string_of_int n))) in
  if sort_digits n1 = sort_digits n2 then true else false

let solve start const_val =
  let rec aux i =
    let rec aux' n ulimit =
      let rec cmp_loop fn cnt =
        if cnt = 1 then true else
          match fn n (n * cnt) with
          | true -> cmp_loop fn (pred cnt)
          | false -> false
      in
      if n > ulimit then
        None
      else
        if cmp_loop cmp_contained_digits const_val then Some n else aux' (succ n) ulimit
    in
    match aux' i (10 * i / const_val) with
    | None -> aux (10 * i)
    | Some v -> v
  in
  aux start

let () =
  Printf.printf "Answer: %d\n" (solve 1 6)
