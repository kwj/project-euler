(* Project Euler: Problem 61 *)

open Core

let make_polygonal_tbl () =
  let make_polygonal_nums i =
    let fn = [|(fun _ -> 0); (fun _ -> 1); (fun n -> n);  (* not used *)
               (fun n -> n * (n + 1) / 2);                (* triangular number *)
               (fun n -> n * n);                          (* square number *)
               (fun n -> n * (3 * n - 1) / 2);            (* pentagonal number *)
               (fun n -> n * (2 * n - 1));                (* hexagonal number *)
               (fun n -> n * (5 * n - 3) / 2);            (* heptagonal number *)
               (fun n -> n * (3 * n - 2))|]               (* octagonal number *)
    in
    let rec aux n acc =
      let poly_num = fn.(i) n in
      if poly_num >= 10_000 then
        List.rev acc |> List.drop_while ~f:(fun x -> x < 1_000)
      else
        aux (succ n) (poly_num :: acc)
    in
    aux 1 []
  in
  let tbl = Array.create ~len:9 [] in
  for i = 3 to 8 do
    tbl.(i) <- (make_polygonal_nums i |> List.map ~f:(fun n -> (n / 100, n mod 100)) |> List.filter ~f:(fun (_, x) -> x >= 10))
  done;
  tbl

let find_chain route_info p_tbl =
  let get_next_chains chain i =
    let key = snd (List.hd_exn chain) in
    List.filter ~f:(fun (x, _) -> x = key) p_tbl.(i)
    |> List.map ~f:(fun pair -> pair :: chain)
  in
  let rec aux chains route =
    match chains, route with
    | [], _ -> []
    | _, [] -> List.filter chains
                           ~f:(fun lst -> let (p1, p2) = List.hd_exn lst in
                                          let (q1, q2) = List.last_exn lst in
                                          p1 = q1 && p2 = q2)
               |> List.map ~f:List.tl_exn
    | _, x :: xs -> aux (List.concat (List.map ~f:(fun lst -> get_next_chains lst x) chains)) xs
  in
  aux (List.map ~f:(fun p -> [p]) p_tbl.(8)) route_info

let solve () =
  let p_tbl = make_polygonal_tbl () in
  let trans_lst = List.map ~f:(fun l -> l @ [8]) (Euler.Util.permutation 5 [3; 4; 5; 6; 7]) in

  let cycles = List.concat (List.map ~f:(fun lst -> find_chain lst p_tbl) trans_lst) in
  if List.length cycles <> 1 then
    failwith "abort"
  else
    List.map ~f:(fun (x, y) -> 100 * x + y) (List.hd_exn cycles)
    |> List.fold ~init:0 ~f:(+)

let exec () =
  Int.to_string (solve ())

let () = Euler.Task.run exec