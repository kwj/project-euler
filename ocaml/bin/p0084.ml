(* Project Euler: Problem 84 *)

(*
  I can't calculate probability from my lack of ability :(.
  So I rolled the dice a million times.

    pos:  0  1  2   3  4  5  6   7  8  9   10 11 12 13 14 15 16  17 18 19
         GO A1 CC1 A2 T1 R1 B1 CH1 B2 B3 JAIL C1 U1 C2 C3 R2 D1 CC2 D2 D3

         20 21  22 23 24 25 26 27 28 29  30 31 32  33 34 35  36 37 38 39
         FP E1 CH2 E2 E3 R3 F1 F2 U2 F3 G2J G1 G2 CC3 G3 R4 CH3 H1 T2 H2
 *)

(* ---------------------------------------------------------------- *)

let cc pos =
  match Random.int 16 with
  | 0 -> 0      (* GO *)
  | 1 -> 10     (* JAIL *)
  | _ -> pos    (* nop *)

let ch pos =
  let next_r i =
    match i with
    | 7 -> 15    (* CH1 -> R2 *)
    | 22 -> 25   (* CH2 -> R3 *)
    | 36 -> 5    (* CH3 -> R1 *)
    | _ -> assert false
  in
  let next_u i =
    match i with
    | 7 -> 12    (* CH1 -> U1 *)
    | 22 -> 28   (* CH2 -> U2 *)
    | 36 -> 12   (* CH3 -> U1 *)
    | _ -> assert false
  in
  match Random.int 16 with
  | 0 -> 0     (* GO *)
  | 1 -> 10    (* JAIL *)
  | 2 -> 11    (* C1 *)
  | 3 -> 24    (* E3 *)
  | 4 -> 39    (* H2 *)
  | 5 -> 5     (* R1 *)
  | 6 -> next_r pos
  | 7 -> next_r pos
  | 8 -> next_u pos
  | 9 -> if pos - 3 >= 0 then (pos - 3) else (pos - 3 + 40)
  | _ -> pos   (* nop *)

let throw_die () = (Random.int 4) + 1

let solve limit =
  let counter = Array.make 40 0 in
  let rec loop n pos double =
    if n = 0 then (
      let result = Array.mapi (fun i elm -> (elm, Printf.sprintf "%02d" i)) counter in
      Array.sort (fun (n1, _) (n2, _) -> n2 - n1) result;
      result
    ) else
      let d1, d2 = throw_die(), throw_die() in
      counter.(pos) <- counter.(pos) + 1;
      if d1 = d2 && double = 2 then
        loop (pred n) 10 0    (* -> JAIL *)
      else
        let new_pos = (pos + d1 + d2) mod 40 in
        match new_pos with
        | 30 ->    (* G2J *)
           if d1 = d2 then
             loop (pred n) 10 (succ double)    (* -> JAIL *)
           else
             loop (pred n) 10 0    (* -> JAIL *)
        | 2 | 17 | 33 ->    (* CC1/CC2/CC3 *)
           if d1 = d2 then
             loop (pred n) (cc new_pos) (succ double)
           else
             loop (pred n) (cc new_pos) 0
        | 7 | 22 | 36 ->    (* CH1/CH2/CH3 *)
           if d1 = d2 then
             loop (pred n) (ch new_pos) (succ double)
           else
             loop (pred n) (ch new_pos) 0
        | _ ->
           if d1 = d2 then
             loop (pred n) new_pos (succ double)
           else
             loop (pred n) new_pos 0
  in
  let result = loop limit 0 0 in
  let (_, s1), (_, s2), (_, s3) = result.(0), result.(1), result.(2) in
  s1 ^ s2 ^ s3

let () =
  Random.self_init();
  Printf.printf "Answer: %s\n" (solve 1_000_000)
