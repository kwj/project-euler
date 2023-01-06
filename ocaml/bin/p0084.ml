(* Project Euler: Problem 84 *)

(*
  I can't calculate probability from my lack of ability :(.
  So I rolled the dice a million times. (Monte Carlo method)

    pos:  0  1  2   3  4  5  6   7  8  9   10 11 12 13 14 15 16  17 18 19
         GO A1 CC1 A2 T1 R1 B1 CH1 B2 B3 JAIL C1 U1 C2 C3 R2 D1 CC2 D2 D3

         20 21  22 23 24 25 26 27 28 29  30 31 32  33 34 35  36 37 38 39
         FP E1 CH2 E2 E3 R3 F1 F2 U2 F3 G2J G1 G2 CC3 G3 R4 CH3 H1 T2 H2
 *)

open Core

type square = GO   | A1 | CC1 | A2  | T1 | R1 | B1  | CH1 | B2 | B3 |
              JAIL | C1 | U1  | C2  | C3 | R2 | D1  | CC2 | D2 | D3 |
              FP   | E1 | CH2 | E2  | E3 | R3 | F1  | F2  | U2 | F3 |
              G2J  | G1 | G2  | CC3 | G3 | R4 | CH3 | H1  | T2 | H2

let get_pos sq =
  match sq with
    GO -> 0    | A1 -> 1   | CC1 -> 2  | A2 -> 3   | T1 -> 4
  | R1 -> 5    | B1 -> 6   | CH1 -> 7  | B2 -> 8   | B3 -> 9
  | JAIL -> 10 | C1 -> 11  | U1 -> 12  | C2 -> 13  | C3 -> 14
  | R2 -> 15   | D1 -> 16  | CC2 -> 17 | D2 -> 18  | D3 -> 19
  | FP -> 20   | E1 -> 21  | CH2 -> 22 | E2 -> 23  | E3 -> 24
  | R3 -> 25   | F1 -> 26  | F2 -> 27  | U2 -> 28  | F3 -> 29
  | G2J -> 30  | G1 -> 31  | G2 -> 32  | CC3 -> 33 | G3 -> 34
  | R4 -> 35   | CH3 -> 36 | H1 -> 37  | T2 -> 38  | H2 -> 39

let get_square pos =
  let tbl = [| GO;   A1; CC1; A2;  T1; R1; B1;  CH1; B2; B3;
               JAIL; C1; U1;  C2;  C3; R2; D1;  CC2; D2; D3;
               FP;   E1; CH2; E2;  E3; R3; F1;  F2;  U2; F3;
               G2J;  G1; G2;  CC3; G3; R4; CH3; H1;  T2; H2 |] in
  tbl.(pos)

(* Community Chest *)
let c_chest sq =
  match Random.int 16 with
    0 -> GO
  | 1 -> JAIL
  | _ -> sq    (* nop *)

(* Chance Card *)
let chance sq =
  (* Railway Company *)
  let next_r i =
    match i with
      CH1 -> R2
    | CH2 -> R3
    | CH3 -> R1
    | _ -> assert false
  in

  (* Utility Company *)
  let next_u i =
    match i with
      CH1 -> U1
    | CH2 -> U2
    | CH3 -> U1
    | _ -> assert false
  in

  match Random.int 16 with
    0 -> GO
  | 1 -> JAIL
  | 2 -> C1
  | 3 -> E3
  | 4 -> H2
  | 5 -> R1
  | 6 -> next_r sq
  | 7 -> next_r sq
  | 8 -> next_u sq
  | 9 -> get_square (((get_pos sq) + 37) mod 40)    (* go back 3 squares *)
  | _ -> sq   (* nop *)

let throw_die () = (Random.int 4) + 1

let monte_calro limit =
  let counter = Array.create ~len:40 0 in
  let rec loop n sq double =
    if n = 0 then (
      let result = Array.mapi ~f:(fun i elm -> (elm, Printf.sprintf "%02d" i)) counter in
      Array.stable_sort ~compare:(fun (n1, _) (n2, _) -> n2 - n1) result;
      result
    ) else
      let d1, d2 = throw_die (), throw_die () in
      counter.(get_pos sq) <- counter.(get_pos sq) + 1;
      if d1 = d2 && double = 2 then
        loop (pred n) JAIL 0
      else
        let next_double = if d1 = d2 then succ double else 0 in
        let next_sq = get_square (((get_pos sq) + d1 + d2) mod 40) in
        match next_sq with
          G2J -> loop (pred n) JAIL next_double
        | CC1| CC2 | CC3 -> loop (pred n) (c_chest next_sq) next_double
        | CH1 | CH2 | CH3 -> loop (pred n) (chance next_sq) next_double
        | _ -> loop (pred n) next_sq next_double
  in
  let result = loop limit GO 0 in
  let (_, s1), (_, s2), (_, s3) = result.(0), result.(1), result.(2) in
  s1 ^ s2 ^ s3

let solve () =
  let rec loop n acc =
    Random.self_init ();
    if n = 0 then
      let rec aux cnt acc = function
          x1 :: (x2 :: _ as xs) when Bool.(String.equal x1 x2 = true) ->
            aux (succ cnt) acc xs
        | x1 :: (_ :: _ as xs) ->
            aux 1 ((cnt, x1) :: acc) xs
        | x :: [] ->
            (cnt, x) :: acc
        | [] ->
            failwith "not reached"
      in
      aux 1 [] (List.sort acc ~compare:String.compare)
      |> List.sort ~compare:(fun (x1, _) (x2, _) -> x2 - x1)
      |> List.hd_exn
    else
      loop (pred n) ((monte_calro 100_000) :: acc)
  in
  loop 100 []

let exec () =
  snd (solve ())

let () = Euler.Task.run exec
