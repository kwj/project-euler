(* Project Euler: Problem 90 *)

open Core

let squares =
  [ [ (1, 0); (0, 1) ]
  ; [ (4, 0); (0, 4) ]
  ; [ (6, 0); (0, 6) ]
  ; [ (6, 1); (1, 6) ]
  ; [ (5, 2); (2, 5) ]
  ; [ (6, 3); (3, 6) ]
  ; [ (6, 4); (4, 6) ]
  (* ; [ (4, 6); (6, 4) ] *)  (* 8^2 = 64, 7^2 = 49 -> 46.  So this check can be omitted. *)
  ; [ (1, 8); (8, 1) ]
  ] [@ocamlformat "disable"]
;;

let check_pair two_dice pair =
  List.mem (List.nth_exn two_dice 0) (fst pair) ~equal
  && List.mem (List.nth_exn two_dice 1) (snd pair) ~equal
;;

let check_square two_dice =
  assert (List.length two_dice = 2);
  List.for_all squares ~f:(fun pair_lst ->
    List.exists pair_lst ~f:(fun pair -> check_pair two_dice pair))
;;

let compute () =
  Euler.Util.combination 6 [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 6 ]
  |> Euler.Util.combination_with_repetition 2
  |> List.count ~f:(fun two_dice -> check_square two_dice)
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Cube Digit Pairs" = [%test_eq: int] (compute ()) 1217
