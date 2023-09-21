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
  ring: int array [size: n_gon * 2 + 1]

          r1
            \
            r0  [r6 <- r0]
           /  \
         r4---r2-r3
         /
       r5

          r1
            \  [r10 <- r0]
            r0  r3
           /  \ /
         r8   r2
         /\   /
      r9 r6-r4-r5
            \
             r7

  bit_mask: 1: used number, 0: unused number
    the following is an example when n_gon = 5.
      0x11111111110
        ^        ^
        10  ...  1
*)

open Core

let is_valid x y bit_mask n_gon =
  0 < x && x <= (n_gon * 2) && 0 < y && y <= (n_gon * 2) && x <> y && ((1 lsl x) lor (1 lsl y)) land bit_mask = 0
;;

let rec dfs n_gon idx bit_mask ring total result =
  if idx = n_gon * 2 - 2
  then (
    let tmp = total - ring.(0) - ring.(idx) in
    if (0 < tmp && tmp <= n_gon * 2) && ((1 lsl tmp) land bit_mask = 0)
    then (
      ring.(idx + 1) <- tmp;
      (* Confirm ring.(1) is the minimum outer node or not *)
      if ring.(1)
        = (List.range 1 (n_gon * 2) ~stop:`exclusive ~stride:2
           |> List.map ~f:(fun idx -> ring.(idx))
           |> List.min_elt ~compare:Int.compare
           |> Option.value_exn)
      then (
        let rec aux s idx =
          let s = Printf.sprintf "%d%d%d%s" ring.(idx + 1) ring.(idx) ring.(idx + 2) s in
          if idx = 0 then s else aux s (idx - 2)
        in
        result := (aux "" idx) :: !result)))
  else (
    let rec loop = function
      | [] -> ()
      | outer_node :: xs ->
        let inner_node = total - ring.(idx) - outer_node in
        if is_valid outer_node inner_node bit_mask n_gon
        then (
          ring.(idx + 1) <- outer_node;
          ring.(idx + 2) <- inner_node;
          dfs
            n_gon
            (idx + 2)
            ((1 lsl outer_node) lor (1 lsl inner_node) lor bit_mask)
            ring
            total
            result);
        loop xs
    in
    loop (List.range 1 (n_gon * 2) ~stop:`inclusive)
  )
;;

let compute n_gon =
  let ring = Array.init (n_gon * 2 + 1) ~f:(fun _ -> 0) in
  let result = ref [] in

  (* The minimum total of the line which contains 'n_gon * 2' is 1 + 2 + (n_gon * 2) = n_gon * 2 + 3. *)
  (* The maximum total of the line which contains '1' is 1 + (n_gon * 2 - 1) + (n_gon * 2) = n_gon * 4. *)
  for total = (n_gon * 2 + 3) to (n_gon * 4) do
    for n = 1 to (n_gon * 2) do
      ring.(0) <- n;
      ring.(n_gon * 2) <- n;
      dfs n_gon 0 (1 lsl n) ring total result
    done
  done;

  (* Only 16-digit strings are covered in this problem (when n_gon == 5). *)
  if n_gon = 5 then result := List.filter ~f:(fun s -> String.length s = 16) !result;

  List.map ~f:(fun s -> Int.of_string s) !result
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let solve () = compute 5 |> Int.to_string

(* Test *)

let%test_unit "3" = [%test_eq: int] (compute 3) 432621513
let%test_unit "5" = [%test_eq: int] (compute 5) 6531031914842725
