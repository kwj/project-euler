(* Project Euler: Problem 43 *)

(*
  - d1d2d3d4d5d6d7d8d9d10 is a pandigital number
  - d2d3d4 is divisible by 2
  - d3d4d5 is divisible by 3
  - d4d5d6 is divisible by 5
  - d5d6d7 is divisible by 7
  - d6d7d8 is divisible by 11
  - d7d8d9 is divisible by 13
  - d8d9d10 is divisible by 17

  step #1
    - d6 is 0 or 5. (because d4d5d6 mod 5 = 0)

  step #2
    - d6d7d8 mod 11 = 0
        d7d8 are {11,22,33,44,55,66,77,88,99 when d6 = 0} --> not pandigital
                                                            so, d6 is always 5
                 {06,17,28,39,61,72,83,94 when d6 = 5}

  step #3
    - d7d8d9 mod 13 = 0
        065 -> NG ('5' is already used as d6)
        17? -> NG (there are no multiplies of 13 in the 170s)
        286
        390
        611 -> NG (not pandigital)
        728
        832
        949 -> NG (not pandigital)

        d6d7d8d9 candidates: 5286, 5390, 5728, 5832

  step #4
    - d8d910 mod 17 = 0
        867
        901
        289
        323 -> NG (not pandigital)

        d6d7d8d9d10 candidates: 52867, 53901, 57289
                   (unused num. 01349  24678  01346)

  step #5
    - d5d6d7 mod 7 = 0
        ?52 -> 952 (the rest are not multiples of 7)
        ?53 -> NG (there are no multiples of 7)
        ?57 -> 357 (the rest are not multiples of 7)

        d5d6d7d8d9d10 candidates: 952867, 357289
                     (unused num. 0134    0146)

  step #6
    - d2d3d4 is divisible by 2
        d4 is even.
          d4d5d6d7d8d9d10 candidates: 0952867, 4952867, 0357289, 4357289, 6357289
                         (unused num. 134      013      146      016      014)

  step #7
    - (d3 + d4 + d5) mod 3 = 0.
         d3d4d5d6d7d8d9d10 candidates: 30952867, 60357289, 06357289
                          (unused num. 14        14        14)

  step #8
    d1d2d3d4d5d6d7d8d9d10
      1430952867, 1460357289, 1406357289, 4130952867, 4160357289, 4106357289

  Answer:
    * (+ 1430952867 1460357289 1406357289 4130952867 4160357289 4106357289)
    16695334890

    I want to display only the answer :p.

    Hmm, I will use stupid brute force solution. It takes time to generate the permutation.
    This program is slow, but it should be faster than manual calculation.
 *)

(* ---------------------------------------------------------------- *)

(*
let remove elem lst =
  List.filter ((<>) elem) lst

let make_perm_lst r obj_lst =
  let rec aux n lst acc result =
    if n = 0 then
      (List.rev acc) :: result
    else
      List.fold_right (fun x y -> aux (pred n) (remove x lst) (x :: acc) y) lst result
  in
  aux r obj_lst [] []

let tmp = make_perm_lst 10 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
*)

let rec make_perm_lst acc other lst =
  match lst with
  | [] -> [acc]
  | hd :: tl ->
     let result = make_perm_lst (hd :: acc) [] (other @ tl) in
     if tl <> [] then
       result @ make_perm_lst acc (hd :: other) tl
     else
       result

let digits_to_int lst =
  List.fold_left (fun i elem -> 10 * i + elem) 0 lst

let rec check_div_property divs lst =
  match divs, lst with
  | x :: xs, _ :: (d1 :: d2 :: d3 :: _ as tl) ->
     if digits_to_int [d1; d2; d3] mod x = 0 then
       check_div_property xs tl
     else
       false
  | _, _ -> true

let solve () =
  make_perm_lst [] [] [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
  |> List.filter (check_div_property [2; 3; 5; 7; 11; 13; 17])
  |> List.map digits_to_int
  |> List.fold_left (+) 0

let () =
  Printf.printf "Answer: %d\n" (solve ())
