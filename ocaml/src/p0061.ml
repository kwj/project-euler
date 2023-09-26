(* Project Euler: Problem 61 *)

open Core

let make_polygonal_tbl () =
  let make_polygonal_nums i =
    let fn =
      [| (fun _ -> 0) (* not used *)
       ; (fun _ -> 1)
       ; (fun n -> n)
       ; (fun n -> n * (n + 1) / 2) (* triangular number *)
       ; (fun n -> n * n) (* square number *)
       ; (fun n -> n * ((3 * n) - 1) / 2) (* pentagonal number *)
       ; (fun n -> n * ((2 * n) - 1)) (* hexagonal number *)
       ; (fun n -> n * ((5 * n) - 3) / 2) (* heptagonal number *)
       ; (fun n -> n * ((3 * n) - 2)) (* octagonal number *)
      |]
    in
    let rec aux n acc =
      let poly_num = fn.(i) n in
      if poly_num < 1_000
      then aux (succ n) acc
      else if poly_num >= 10_000
      then List.rev acc
      else aux (succ n) (poly_num :: acc)
    in
    aux 1 []
  in
  let tbl = Array.create ~len:9 [] in
  for i = 3 to 8 do
    tbl.(i)
    <- make_polygonal_nums i
       |> List.map ~f:(fun n -> (n / 100, n mod 100))
       |> List.filter ~f:(fun (_, x) -> x >= 10)
  done;
  tbl
;;

let find_chain route_info p_tbl =
  let get_next_chains chain i =
    let key = snd (List.hd_exn chain) in
    List.filter ~f:(fun (x, _) -> x = key) p_tbl.(i)
    |> List.map ~f:(fun pair -> pair :: chain)
  in
  let rec aux chains route =
    match (chains, route) with
    | [], _ -> []
    | _, [] ->
      List.filter chains ~f:(fun lst ->
        let p1, p2 = List.hd_exn lst in
        let q1, q2 = List.last_exn lst in
        p1 = q1 && p2 = q2)
      |> List.map ~f:List.tl_exn
    | _, x :: xs ->
      aux (List.concat (List.map ~f:(fun lst -> get_next_chains lst x) chains)) xs
  in
  aux (List.map ~f:(fun p -> [ p ]) p_tbl.(8)) route_info
;;

let compute () =
  let p_tbl = make_polygonal_tbl () in
  let trans_lst =
    List.map ~f:(fun l -> l @ [ 8 ]) (Euler.Util.permutation 5 [ 7; 6; 5; 4; 3 ])
  in

  (*
     From the problem statement:
     - Each elements in cycle is represent by a different number
     - There is only one cycle exist
  *)
  let cycles =
    List.concat (List.map ~f:(fun lst -> find_chain lst p_tbl) trans_lst)
    |> List.map ~f:(fun lst -> List.map ~f:(fun (x, y) -> (100 * x) + y) lst)
    |> List.filter ~f:(fun lst ->
      Bool.(List.contains_dup ~compare:Int.compare lst = false))
  in
  if List.length cycles <> 1
  then failwith "abort"
  else List.sum (module Int) ~f:Fn.id (List.hd_exn cycles)
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Cyclical Figurate Numbers" = [%test_eq: int] (compute ()) 28684
