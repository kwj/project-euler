(* Project Euler: Problem 68 *)

(*
  This problem can be solved with pen and paper.
  No computer is needed.

  1) maximam 16-digit string
    number '10' must be at outer position.

  2) If the condition is met when the numbers '6' to '10' are at
     outer positions, one of them is the answer.

  --> Assume that numbers '6' to '10' are at the outer positions.

  3) the number '1' to '5' are on the pentagon.

  4) the sum of all lines is (1+2+3+4+5)*2 + (6+7+8+9+10) = 70.

  5) the sum of numbers on each lines are all 14.

  6) number '1' and '3' must be on the line which contains '10'.

    case #1       case #2
        10            10
          \             \
           3  8,9        1  6,7,8,9
            \ /           \ /
             1             3

  7) case #1-1:
          10
            \
             3   8
              \ /
               1
              /
             5

       <a>                 <b>
          10              10
            \               \
             3   8           3   8
           /  \ /          /  \ /
          4    1   [NG]   2    1    [NG]
         /\   /           \   /
       [7] 2-5--7          4-5-[5]
            \
             8

     case #1-2:
          10
            \
             3   9
              \ /
               1
              /
             4

       <a>                 <b>
          10                10
            \                 \
             3   9             3   9
              \ /            /  \ /
               1   [NG]     5    1   [OK] 6,5,3;10,3,1;9,1,4;8,4,2;7,2,5
              /            /\   /
           5-4-[5]        6  2-4--8
                              \
                               7

  8) case #2-1:
          10
            \
             1   9
              \ /
               3
              /
             2
       <a>                 <b>
          10                   10
            \                    \
             1   9                1   9
           /  \ /               /  \ /
          5    3   [NG]        4    3  [NG]
          \   /                \   /
           4-2                  5-2
            \                    \
            [5]                  [5]

     case #2-2:
          10
            \
             1   8
              \ /
               3   [NG]
              /
            [3]

     case #2-3:
          10
            \
             1   7
              \ /
               3
              /
             4

       <a>                 <b>
          10                   10
            \                    \
             1   7                1   7
           /  \ /               /  \ /
          5    3   [NG]        2    3  [NG]
          \   /                \   /
           2-4                  5-4
            \                    \
            [7]                  [7]

     case #2-4:
          10
            \
             1   6
              \ /
               3
              /
             5

       <a>                 <b>
          10                   10
            \                    \
             1   6                1   6
           /  \ /               /  \ /
          2    3   [NG]        4    3   [OK] 6,3,5;7,5,2;8,2,4;9,4,1;10,1;3
          \   /               /\   /
           4-5-[5]           9  2-5-7
                                 \
                                  8

  9) There are two patterns which satisfy the condition.
     The answer is case #1-2 <b>.
      case #1-2 <b>
        6,5,3;10,3,1;9,1,4;8,4,2;7,2,5
      case #2-4 <b>
        6,3,5;7,5,2;8,2,4;9,4,1;10,1;3
 *)

(*
        <e1>
          a0
            \   <e2>
            a1  a3
           /  \ /
         a8   a2
         /\   /
       a9 a6-a4-a5 <e3>
     <e5>   \
             a7
             <e4>

  list [a0; a1; a2; a3; a4; a5; a6; a7; a8; a9]
   - one of {a3, a5, a7, a9} is 10
   - a0 < {a3, a5, a7, a9}
   - each lines (e1..e5) have same weight

  I'll use brute force with the above conditions.
 *)

open Core

let check_10 lst =
  List.nth_exn lst 3 = 10 || List.nth_exn lst 5 = 10 || List.nth_exn lst 7 = 10 || List.nth_exn lst 9 = 10

let check_outer lst =
  List.nth_exn lst 0 < List.nth_exn lst 3 && List.nth_exn lst 0 < List.nth_exn lst 5 &&
    List.nth_exn lst 0 < List.nth_exn lst 7 && List.nth_exn lst 0 < List.nth_exn lst 9

let check_weight lst =
  let e1 = (List.nth_exn lst 0) + (List.nth_exn lst 1) + (List.nth_exn lst 2) in
  let e2 = (List.nth_exn lst 2) + (List.nth_exn lst 3) + (List.nth_exn lst 4) in
  let e3 = (List.nth_exn lst 4) + (List.nth_exn lst 5) + (List.nth_exn lst 6) in
  let e4 = (List.nth_exn lst 6) + (List.nth_exn lst 7) + (List.nth_exn lst 8) in
  let e5 = (List.nth_exn lst 8) + (List.nth_exn lst 9) + (List.nth_exn lst 1) in
  e1 = e2 && e2 = e3 && e3 = e4 && e4 = e5

let check_conditions lst =
  check_10 lst && check_outer lst && check_weight lst

let intlst_to_str lst =
  match List.map lst ~f:Int.to_string with
  | a0 :: a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: [] ->
     List.fold ~init:"" ~f:(^) [a0; a1; a2; a3; a2; a4; a5; a4; a6; a7; a6; a8; a9; a8; a1]
  | _ -> assert false

let solve () =
  let perm = Euler.Aw_perm.generator [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  let rec loop acc =
    match perm () with
    | None -> acc
    | Some l -> if check_conditions l then loop (l :: acc) else loop acc
  in
  loop []
  |> List.map ~f:intlst_to_str
  |> List.sort ~compare:String.descending
  |> List.hd_exn

let exec () = solve ()

let () = Euler.Task.run exec
