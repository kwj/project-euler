(* Project Euler: Problem 84 *)

(*
  Stochastic matrix: stoch_matrix

   Current: S0       S1       S2
        +--------+--------+--------+ State:
  Next  |        |        |        |  S0: no doubles
   S0   |  s00   |  s10   |  s20   |  S1: one doubles occurred
        |        |        |        |  S2: two consecutive dobules occurred
        +--------+--------+--------+
  Next  |        |        |        | s00: transition probability if no doubles occurred (the next state is S0)
   S1   |  s01   | s11=0  | s21=0  | s01: transition probability if fist doubles occurred (the next state is S1)
        |        |        |        |
        +--------+--------+--------+ s10: transition probability if no doubles occurred (the next state is S0)
  Next  |        |        |        | s12: transition probability if two consecutive doubles occurred (the next state is S2)
   S2   | s02=0  |  s12   | s22=0  |
        |        |        |        | s20: transition probability if both doubles and no doubles occurred (the next state is S0)
        +--------+--------+--------+      Note: go to JAIL@S0 if three consecutive doubles occurred

  s##:
         | GO  A1  CC1 ... [current square]
    -----+---------------------------
     GO  | ##  ##  ##  ...
     A1  | ##  ##  ##  ...
     CC1 | ##  ##  ##  ...
      .  |  .   .   .
  [next square]
*)

open Core

let sq_GO = 0
and sq_CC1 = 2
and sq_R1 = 5
and sq_CH1 = 7
and sq_JAIL = 10
and sq_C1 = 11
and sq_U1 = 12
and sq_R2 = 15
and sq_CC2 = 17
and sq_CH2 = 22
and sq_E3 = 24
and sq_R3 = 25
and sq_U2 = 28
and sq_G2J = 30
and sq_CC3 = 33
and sq_CH3 = 36
and sq_H2 = 39

let blit_matrix src dst =
  let n_rows = Array.length src in
  let n_cols = Array.length src.(0) in
  List.range 0 n_rows ~stop:`exclusive
  |> List.iter ~f:(fun row ->
    Array.blit ~src:src.(row) ~src_pos:0 ~dst:dst.(row) ~dst_pos:0 ~len:n_cols)
;;

let is_steady arr1 arr2 =
  assert (Array.length arr1 = Array.length arr2);
  Array.for_alli arr1 ~f:(fun idx a1 -> Float.(abs (a1 -. arr2.(idx)) < epsilon_float))
;;

let go_steady matrix =
  assert (Array.length matrix = Array.length matrix.(0));
  let size = Array.length matrix in
  let prev = Array.make_matrix ~dimx:size ~dimy:size 0. in
  let work = Array.make_matrix ~dimx:size ~dimy:size 0. in

  (* To converge quickly, use property of exponentiation. *)
  (*   M^{n+n} = M^{n} * M^{n}*)
  blit_matrix matrix work;
  let rec loop () =
    blit_matrix work prev;
    for x = 0 to size - 1 do
      for y = 0 to size - 1 do
        work.(x).(y)
          <- List.range 0 (size - 1) ~stop:`inclusive
             |> List.map ~f:(fun i -> prev.(x).(i) *. prev.(i).(y))
             |> List.sum (module Float) ~f:Fn.id
      done
    done;
    if is_steady work.(0) prev.(0)
    then work
    else loop ()
  in
  loop ()
;;

let compute nfaces nsquares =
  let dice_prblty_without_dbl = Array.create ~len:(nfaces * 2 + 1) 0. in
  let dice_prblty_dbl = Array.create ~len:(nfaces * 2 + 1) 0. in

  for n1 = 1 to nfaces do
    for n2 = 1 to nfaces do
      if n1 <> n2
      then
        dice_prblty_without_dbl.(n1 + n2)
          <- dice_prblty_without_dbl.(n1 + n2) +. (1.0 /. Float.of_int (nfaces * nfaces))
      else
        dice_prblty_dbl.(n1 + n2)
          <- dice_prblty_dbl.(n1 + n2) +. (1.0 /. Float.of_int (nfaces * nfaces))
    done
  done;

  let stoch_matrix = Array.make_matrix ~dimx:120 ~dimy:120 0. in

  (* no doubles occured. setup 's00', 's10' and 's20' *)
  for x = 0 to 119 do
    for v = 2 to nfaces * 2 do
      stoch_matrix.((x + v) mod 40).(x) <- dice_prblty_without_dbl.(v)
    done
  done;

  (* doubles occured. setup 's01' *)
  for x = 0 to 39 do
    for v = 2 to nfaces * 2 do
      stoch_matrix.(((x + v) mod 40) + 40).(x) <- dice_prblty_dbl.(v)
    done
  done;
  (* setup 's12' *)
  for x = 40 to 79 do
    for v = 2 to nfaces * 2 do
      stoch_matrix.(((x + v) mod 40) + 80).(x) <- dice_prblty_dbl.(v)
    done
  done;
  (* modify 's20' *)
  for x = 80 to 119 do
    stoch_matrix.(sq_JAIL).(x) <- Array.sum (module Float) dice_prblty_dbl ~f:Fn.id
  done;

  (* Goto Jail *)
  List.iter [ 0; 40; 80 ] ~f:(fun offset ->
    for x = 0 to 119 do
      stoch_matrix.(sq_JAIL).(x)
        <- stoch_matrix.(sq_JAIL).(x) +. stoch_matrix.(sq_G2J + offset).(x);
      stoch_matrix.(sq_G2J + offset).(x) <- 0.0
    done);

  (* Chance Card *)
  (* note: It must be processed before Communy Chest because the CH3 -> CC3 path is exist. *)
  List.iter [ 0; 40; 80 ] ~f:(fun offset ->
    List.iter [ sq_CH1; sq_CH2; sq_CH3 ] ~f:(fun chance ->
      Array.to_list stoch_matrix.(chance + offset)
      |> List.iteri ~f:(fun current prblty ->
        let next_r =
          if chance = sq_CH1 then sq_R2 else if chance = sq_CH2 then sq_R3 else sq_R1
        in
        let next_u = if chance = sq_CH2 then sq_U2 else sq_U1 in
        List.iter
          [ sq_GO
          ; sq_C1
          ; sq_E3
          ; sq_H2
          ; sq_R1
          ; next_r
          ; next_r
          ; next_u
          ; (chance - 3) mod 40
          ]
          ~f:(fun next_sq ->
            stoch_matrix.(next_sq + offset).(current)
              <- stoch_matrix.(next_sq + offset).(current) +. (prblty /. 16.));
        stoch_matrix.(sq_JAIL).(current)
          <- stoch_matrix.(sq_JAIL).(current) +. (prblty /. 16.);
        stoch_matrix.(chance).(current)
          <- stoch_matrix.(chance).(current) -. ((prblty /. 16.) *. 10.))));

  (* Community Chest *)
  List.iter [ 0; 40; 80 ] ~f:(fun offset ->
    List.iter [ sq_CC1; sq_CC2; sq_CC3 ] ~f:(fun chest ->
      for x = 0 to 119 do
        stoch_matrix.(sq_GO + offset).(x)
          <- stoch_matrix.(sq_GO + offset).(x)
             +. (stoch_matrix.(chest + offset).(x) /. 16.0);
        stoch_matrix.(sq_JAIL).(x)
          <- stoch_matrix.(sq_JAIL).(x) +. (stoch_matrix.(chest + offset).(x) /. 16.0);
        stoch_matrix.(chest + offset).(x)
          <- stoch_matrix.(chest + offset).(x) -. (stoch_matrix.(chest + offset).(x) /. 8.0)
      done));

  let steady_state = go_steady stoch_matrix in
  List.range 0 39 ~stop:`inclusive
  |> List.map ~f:(fun i ->
    (Printf.sprintf "%02d" i,
     steady_state.(i).(0) +. steady_state.(i + 40).(0) +. steady_state.(i + 80).(0)))
  |> List.sort ~compare:(fun (_, f1) (_, f2) -> Float.compare f2 f1)
  |> Fn.flip List.take nsquares
  |> Euler.Util.list_to_str (fun (sq, _) -> sq) ""
;;

let solve () = compute 4 3

(* Test *)

let%test_unit "4, 3" = [%test_eq: string] (compute 4 3) "101524"
let%test_unit "6, 3" = [%test_eq: string] (compute 6 3) "102400"
