(* Project Euler: Problem 93 *)

(*
  Infix notation and Reverse Polish notation

    case 1:
       IN: ((x1 OP1 x2) OP2 x3) OP3 x4
      RPN: x1 x2 OP1 x3 OP2 x4 OP3

    case 2:
       IN: (x1 OP1 (x2 OP2 x3)) OP3 x4
      RPN: x1 x2 x3 OP2 OP1 x4 OP3

    case 3:
       IN: (x1 OP1 x2) OP2 (x3 OP3 x4)
      RPN: x1 x2 OP1 x3 x4 OP3 OP2

    case 4:
       IN: x1 OP1 ((x2 OP2 x3) OP3 x4)
      RPN: x1 x2 x3 OP2 x4 OP3 OP1

    case 5:
       IN: x1 OP1 (x2 OP2 (x3 OP3 x4))
      RPN: x1 x2 x3 x4 OP3 OP2 OP1
 *)

(* ---------------------------------------------------------------- *)

open Core

type element = Num of Q.t | Op of (Q.t -> Q.t -> Q.t)

let rep_perm r lst =
  let rec aux ys n xs acc =
    if n <= 0 then
      ys :: acc
    else
      List.fold_right ~init:acc ~f:(fun x -> aux (x :: ys) (n - 1) xs) xs
  in
  aux [] r lst []                                         

let op_lst = rep_perm 3 [Op Q.add; Op Q.sub; Op Q.mul; Op Q.div]
             |> List.map ~f:(fun l -> (List.nth_exn l 0, List.nth_exn l 1, List.nth_exn l 2))

let make_rpn_lst (a, b, c, d) =
  let rec loop_num result = function
      [] -> result
    | [x1; x2; x3; x4] :: xs ->
        let rec aux acc = function
            [] -> acc
          | (op1, op2, op3) :: ops ->
              aux ([x1; x2; op1; x3; op2; x4; op3] ::    (* case 1 *)
                   [x1; x2; x3; op2; op1; x4; op3] ::    (* case 2 *)
                   [x1; x2; op1; x3; x4; op3; op2] ::    (* case 3 *)
                   [x1; x2; x3; op2; x4; op3; op1] ::    (* case 4 *)
                   [x1; x2; x3; x4; op3; op2; op1] ::    (* case 5 *)
                   acc) ops
        in
        loop_num (aux result op_lst) xs
    | _ -> assert false
  in
  loop_num [] (Euler.Util.permutation 4 [Num (Q.of_int a); Num (Q.of_int b); Num (Q.of_int c); Num (Q.of_int d)])


let make_num_tpl_lst () =
  let lst = ref [] in
  for a = 1 to 6 do
    for b = a + 1 to 7 do
      for c = b + 1 to 8 do
        for d = c + 1 to 9 do
          lst := (a, b, c, d) :: !lst
        done
      done
    done
  done;
  !lst

let calc_rpn_lst rpn_lst =
  let rec loop acc = function
    [] ->
      let tmp = List.hd_exn acc in
      if Z.(equal (Q.den tmp) one) then Z.to_int (Q.num tmp) else 0
  | x :: xs ->
      match x with
        Num v -> loop (v :: acc) xs
      | Op op -> loop (op (List.nth_exn acc 0) (List.nth_exn acc 1) :: (List.tl_exn (List.tl_exn acc))) xs
  in
  loop [] rpn_lst

let find_consecutive lst =
  let rec loop = function
    | a :: (b :: _ as xs) when a + 1 = b -> loop xs
    | a :: _ -> a
    | [] -> assert false
  in
  loop lst

let solve () =
  let module PQ = Euler.PrioQueue.Make(struct
                      type t = int * (int * int * int * int)
                      let compare x y = Int.compare (fst x) (fst y)
                    end) in
  let pq = PQ.init () in

  let rec loop lst =
    match lst with
      [] -> PQ.peek pq
    | tpl :: xs ->
        let v = List.map ~f:calc_rpn_lst (make_rpn_lst tpl)
                |> List.filter ~f:((<) 0)
                |> List.dedup_and_sort ~compare
                |> find_consecutive in
        PQ.insert pq (v, tpl);
        loop xs
  in
  loop (make_num_tpl_lst ())

let exec () =
  let _, (a, b, c, d) = solve () in
  sprintf "%d%d%d%d" a b c d

let () = Euler.Task.run exec
