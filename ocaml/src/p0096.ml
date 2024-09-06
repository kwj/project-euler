(* Project Euler: Problem 96 *)

(*
 * backtracking version (using Algorithm X)
 *
 * Algorithm X (using Dancing Links)
 *   https://en.wikipedia.org/wiki/Algorithm_X
 *   https://en.wikipedia.org/wiki/Dancing_Links
 *
 * library: algo_x (https://github.com/kwj/algo_x)
 *
 * [Grid]
 *     C0 C1 C2 C3 C4 C5 C6 C7 C8
 *    +--------+--------+--------+    R: Row
 *  R0|        |        |        |    C: Column
 *  R1|   B0   |   B1   |   B2   |    B: Box
 *  R2|        |        |        |
 *    +--------+--------+--------+    Cell(x,y) = RxCy
 *  R3|        |        |        |      position = 9x + y
 *  R4|   B3   |   B4   |   B5   |
 *  R5|        |        |        |
 *    +--------+--------+--------+
 *  R6|        |        |        |
 *  R7|   B6   |   B7   |   B8   |
 *  R8|        |        |        |
 *    +--------+--------+--------+
 *
 * [Matrix]
 *        R0C0 R0C1 .. R8C8 | R0#1 R0#2 .. R8#8 R8#9 | C0#1 C0#2 .. C8#8 C8#9 | B0#1 B0#2 .. B8#8 B8#9
 * ----------------------------------------------------------------------------------------------------
 * R0C0#1   1    0  ..   0      1    0  ..   0    0      1    0  ..   0    0      1    0  ..   0    0
 * R0C0#2   1    0  ..   0      0    1  ..   0    0      0    1  ..   0    0      0    1  ..   0    0
 *  ...
 * R0C0#9   1    0  ..   0      0    0  ..   0    0      0    0  ..   0    0      0    0  ..   0    0
 * R0C1#1   0    1  ..   0      1    0  ..   0    0      0    0  ..   0    0      1    0  ..   0    0
 *  ...
 * R0C8#9   0    0  ..   0      0    0  ..   0    0      0    0  ..   0    1      0    0  ..   0    0
 * R1C0#1   0    0  ..   0      0    0  ..   0    0      1    0  ..   0    0      0    0  ..   0    0
 *  ...
 * R8C8#8   0    0  ..   1      0    0  ..   1    0      0    0  ..   1    0      0    0  ..   1    0
 * R8C8#9   0    0  ..   1      0    0  ..   0    1      0    0  ..   0    1      0    0  ..   0    1
 *
 *   Row: RxCy#N -> Cell(x,y) = N
 *   Col: RxCy -> some number is in Cell(x,y)       [cell constraint] (81 columns)
 *        Rx#N -> number 'N' is in the row Rx       [row constraint] (81 columns)
 *        Cy#N -> number 'N' is in the column Cy    [column constraint] (81 columns)
 *        Bz#N -> number 'N' is in the box Bz       [box constraint] (81 columns)
 *
 * We will need the following files to run this program.
 *   - https://projecteuler.net/project/resources/p096_sudoku.txt
 *)

open Core

let parse_data data =
  let trim s =
    Str.global_replace (Str.regexp "[^0-9.]") "" s
    |> Str.global_replace (Str.regexp "\\.") "0"
  in
  let rec loop lines acc result =
    match lines with
    | [] ->
      List.rev (List.map ~f:trim (List.fold ~f:( ^ ) ~init:"" (List.rev acc) :: result))
    | x :: xs ->
      if Str.string_match (Str.regexp "^[0-9.]") x 0
      then loop xs (x :: acc) result
      else if Str.string_match (Str.regexp "^-") x 0 || List.length acc = 0
      then loop xs acc result
      else loop xs [] (List.fold ~f:( ^ ) ~init:"" (List.rev acc) :: result)
  in
  loop data [] []
;;

let make_dlx q =
  (* number of columns = 324 (81 + 81 + 81 + 81) *)
  let d = Euler.Algo_x.dlx_init 324 in
  let add_row pos n =
    let r, c, b = (pos / 9, pos mod 9, (pos / 27 * 3) + (pos mod 9 / 3)) in
    let add_row' num =
      let tag = Printf.sprintf "R%dC%d#%d" r c num in
      Euler.Algo_x.dlx_add_row
        ~name:tag
        d
        [ pos + 1
        ; 81 + 1 + (r * 9) + (num - 1)
        ; (81 * 2) + 1 + (c * 9) + (num - 1)
        ; (81 * 3) + 1 + (b * 9) + (num - 1)
        ]
    in
    if n <> 0
    then add_row' n
    else
      for i = 1 to 9 do
        add_row' i
      done
  in
  List.iteri
    ~f:(fun i n -> add_row i n)
    (List.map ~f:Int.of_string (Str.split (Str.regexp "") q));
  d
;;

let compute str_lst =
  let pick_num lst =
    let rec loop n l acc =
      if n = 0
      then acc
      else (
        match l with
        | [] -> assert false
        | x :: xs ->
          loop
            (pred n)
            xs
            ((acc * 10)
             + Int.of_string (List.hd_exn (List.rev (Str.split (Str.regexp "") x)))))
    in
    loop 3 lst 0
  in
  let rec loop q i acc =
    match q with
    | [] -> acc
    | x :: xs ->
      let d = make_dlx x in
      (match Euler.Algo_x.dlx_solve d with
       | None -> loop xs (succ i) acc
       | Some l ->
         let ans = pick_num (List.sort ~compare:String.compare (List.hd_exn l)) in
         loop xs (succ i) (acc + ans))
  in
  loop (parse_data str_lst) 1 0
;;

let solve fname = compute (Euler.Task.read_file fname) |> Int.to_string

(* Test *)

let%test_unit "p096_roman.txt" =
  [%test_eq: int] (compute (Euler.Task.read_file "./assets/p096_sudoku.txt")) 24702
;;
