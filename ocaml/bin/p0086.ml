(* Project Euler: Problem 86 *)

(*
  1 <= a, b, c <= M

  since we can ignore rotations, there is only one case to consider.
    1 <= a <= b <= c <= M
     --> 2 <= a + b <= 2c

      +--------F
      |        |      * sqrt(c^2 + (a+b)^2) must be an integer
      |--------|
      |        | a+b >= 2
      |        |
      S--------+
           c

  when a+b <= c <= M
    write a+b = x
      (a, b) = (1, x-1), (2, x-2), ..., (x-1, 1)
    however, because a<=b
      num of (a,b) = floor(x/2) = floor((a+b)/2)

  when a+b > c
      num of (a,b) = floor((a+b)/2) - ((a+b) - (c+1))
 *)

(* ---------------------------------------------------------------- *)

let sq_tbl = Hashtbl.create 1024

let sq_memo n =
  match Hashtbl.find_opt sq_tbl n with
  | Some v -> v
  | None ->
     let tmp = n * n in
     Hashtbl.add sq_tbl n tmp;
     tmp

let solve limit =
  let rec loop c acc =
    if acc >= limit then
      c - 1
    else
      let rec loop' a_b acc =
        if a_b < 2 then
          acc
        else
          let tmp = (sq_memo c) + (sq_memo (a_b)) in
          let sqrt_tmp = truncate @@ sqrt @@ float tmp in
          if tmp = sq_memo sqrt_tmp then
            if a_b <= c then
              loop' (pred a_b) (acc + ((a_b) / 2))
            else
              loop' (pred a_b) (acc + ((a_b) / 2) - (a_b - (c + 1)))
          else
            loop' (pred a_b) acc
      in
      loop (succ c) (acc + loop' (c * 2) 0)
  in
  loop 1 0

let () =
  Printf.printf "Answer: %d\n" (solve 1_000_000)
