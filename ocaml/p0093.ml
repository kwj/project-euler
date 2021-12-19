(* Project Euler: Problem 93 *)

(*
  Infix notation and Reverse Polish notation

    case 1:
       IN: ((a1 OP1 a2) OP2 a3) OP3 a4
      RPN: a1 a2 OP1 a3 OP2 a4 OP3

    case 2:
       IN: (a1 OP1 (a2 OP2 a3)) OP3 a4
      RPN: a1 a2 a3 OP2 OP1 a4 OP3

    case 3:
       IN: (a1 OP1 a2) OP2 (a3 OP3 a4)
      RPN: a1 a2 OP1 a3 a4 OP3 OP2

    case 4:
       IN: a1 OP1 ((a2 OP2 a3) OP3 a4)
      RPN: a1 a2 a3 OP2 a4 OP3 OP1

    case 5:
       IN: a1 OP1 (a2 OP2 (a3 OP3 a4))
      RPN: a1 a2 a3 a4 OP3 OP2 OP1
 *)

(* ---------------------------------------------------------------- *)

let reperm r lst =
  let rec aux ys n xs acc =
    if n <= 0 then
      ys :: acc
    else
      List.fold_right (fun x -> aux (x :: ys) (n - 1) xs) xs acc
  in
  aux [] r lst []                                         

let rec perm r lst =
  let rec insert x lst =
    match lst with
    | [] -> [[x]]
    | hd::tl -> (x::lst) :: (List.map (fun l -> hd::l) (insert x tl))
  in
  if r = 1 then
    List.map (fun e -> [e]) lst
  else
    match lst with
    | [] -> []
    | hd::tl -> List.append
                  (List.concat (List.map (fun e -> (insert hd e)) (perm (pred r) tl)))
                  (perm r tl)

type element = Number of Q.t | Operator of (Q.t -> Q.t -> Q.t)

let op_lst = reperm 3 [Operator Q.add; Operator Q.sub; Operator Q.mul; Operator Q.div]

let make_rpn_lst a b c d =
  let rec loop_n nlst acc =
    match nlst with
    | [] -> acc
    | hd :: tl ->
       let rec loop_o ns olst acc =
         match olst with
         | [] -> acc
         | ops :: tl ->
            loop_o ns tl
              ( (* case 1 *)
               [List.nth ns 0; List.nth ns 1; List.nth ops 0; List.nth ns 2; List.nth ops 1; List.nth ns 3; List.nth ops 2]
                (* case 2 *)
               :: [List.nth ns 0; List.nth ns 1; List.nth ns 2; List.nth ops 1; List.nth ops 0; List.nth ns 3; List.nth ops 2]
                (* case 3 *)
               :: [List.nth ns 0; List.nth ns 1; List.nth ops 0; List.nth ns 2; List.nth ns 3; List.nth ops 2; List.nth ops 1]
                (* case 4 *)
               :: [List.nth ns 0; List.nth ns 1; List.nth ns 2; List.nth ops 1; List.nth ns 3; List.nth ops 2; List.nth ops 0]
                (* case 5 *)
               :: [List.nth ns 0; List.nth ns 1; List.nth ns 2; List.nth ns 3; List.nth ops 2; List.nth ops 1; List.nth ops 0]
               :: acc)
       in
       loop_n tl (loop_o hd op_lst acc)
  in
  loop_n (perm 4 [Number (Q.of_int a); Number (Q.of_int b); Number (Q.of_int c); Number (Q.of_int d)]) []

let calc_rpn lst =
  let rec loop elms acc =
    match elms with
    | [] ->
       let tmp = List.hd acc in
       if Q.classify tmp <> NZERO || Q.den tmp <> Z.one then 0 else Z.to_int (Q.num tmp)
    | hd :: tl ->
       match hd with
       | Number v -> loop tl (v :: acc)
       | Operator op -> loop tl (op (List.nth acc 0) (List.nth acc 1) :: (List.tl (List.tl acc)))
  in
  loop lst []

let rec uniq lst =
  match lst with
  | [] -> []
  | hd1 :: (hd2 :: _ as tl) when hd1 = hd2 -> uniq tl
  | hd :: tl -> hd :: (uniq tl)

let find_consecutive lst =
  let rec loop = function
    | a :: (b :: _ as tl) when a + 1 = b -> loop tl
    | a :: tl -> a
    | [] -> assert false
  in
  loop lst

let solve () =
  let acc = ref [] in
  for a = 1 to 6 do
    for b = a + 1 to 7 do
      for c = b + 1 to 8 do
        for d = c + 1 to 9 do
          let consec_num = List.map calc_rpn (make_rpn_lst a b c d)
                           |> List.filter ((<) 0)
                           |> List.sort compare
                           |> uniq
                           |> find_consecutive in
          acc := (consec_num, Printf.sprintf "%d%d%d%d" a b c d) :: !acc
        done
      done
    done
  done;
  List.sort (fun (n1, _) (n2, _) -> n2 - n1) !acc |> List.hd

let () =
  let _, numstr = solve() in
  Printf.printf "Answer: %s\n" numstr
