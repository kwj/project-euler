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
    let aux n =
      let f = fn.(n) in
      Sequence.(
        unfold ~init:1 ~f:(fun i -> Some (f i, succ i))
        |> drop_while ~f:(fun x -> x < 1_000)
        |> take_while ~f:(fun x -> x < 10_000)
        |> to_list)
    in
    aux i
  in
  let tbl = Array.create ~len:9 [] in
  for i = 3 to 8 do
    tbl.(i)
    <- make_polygonal_nums i
       |> List.filter_map ~f:(fun n ->
         let a, b = (n / 100, n mod 100) in
         if b >= 10 then Some (a, b) else None)
  done;
  tbl
;;

let find_chain route_info p_tbl =
  let get_next_chains chain i =
    let key = snd (List.hd_exn chain) in
    List.filter_map
      ~f:(fun ((x, _) as pair) -> if x = key then Some (pair :: chain) else None)
      p_tbl.(i)
  in
  let rec aux chains route =
    match (chains, route) with
    | [], _ -> []
    | _, [] ->
      List.(
        filter chains ~f:(fun lst ->
          let p1, p2 = hd_exn lst in
          let q1, q2 = last_exn lst in
          p1 = q1 && p2 = q2)
        |> map ~f:tl_exn)
    | _, x :: xs -> aux List.(concat (map ~f:(Fun.flip get_next_chains x) chains)) xs
  in
  aux List.(map ~f:singleton p_tbl.(8)) route_info
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
    List.(
      concat_map ~f:(Fun.flip find_chain p_tbl) trans_lst
      |> map ~f:(map ~f:(fun (x, y) -> (100 * x) + y))
      |> filter ~f:(Fun.compose not (contains_dup ~compare:Int.compare)))
  in
  if List.length cycles <> 1
  then failwith "abort"
  else List.(reduce_exn (hd_exn cycles) ~f:( + ))
;;

let solve () = compute () |> Int.to_string

(* Test *)

let%test_unit "Cyclical Figurate Numbers" = [%test_eq: int] (compute ()) 28684
