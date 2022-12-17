(* Project Euler: Problem 83 *)

(*
  I used Dijkstra's algorithm to solve.
    https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

  You will need the following files to run this program.
    - https://projecteuler.net/project/resources/p083_matrix.txt
 *)

(* ---------------------------------------------------------------- *)

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module Make_bintree(M: Comparable) = struct
  type 'a tree = | Node of 'a * 'a tree * 'a tree
                 | Nil

  let create () = Nil

  let rec search node x =
    match node with
    | Nil ->
       None
    | Node (y, _, _) when M.compare x y = 0 ->
       Some y
    | Node (y, left, _) when M.compare x y < 0 ->
       search left x
    | Node (_, _, right) ->
       search right x

  let rec insert node x =
    match node with
    | Nil ->
       Node (x, Nil, Nil)
    | Node (y, left, right) when M.compare x y < 0 ->
       Node (y, (insert left x), right)
    | Node (y, left, right) ->
       Node (y, left, (insert right x))

  let rec find_min = function
    | Nil ->
       failwith "find_min"
    | Node (x, Nil, _) ->
       x
    | Node (_, left, _) ->
       find_min left

  let rec delete_min = function
    | Nil ->
       failwith "delete_min"
    | Node (x, Nil, right) ->
       right
    | Node (x, left, right) ->
       Node (x, (delete_min left), right)

  let rec delete node x =
    match node with
    | Nil ->
       raise Not_found
    | Node (y, left, right) ->
       if M.compare x y = 0 then
         if left = Nil then
           right
         else
           if right = Nil then
             left
           else
             let min_elm = find_min right in
             Node (min_elm, left, (delete_min right))
       else
         if M.compare x y > 0 then
           Node (y, left, (delete right x))
         else
           Node (y, (delete left x), right)

  let is_empty = function
    | Nil -> true
    | _ -> false
end

let read_data() =
  let filename = ref "" in
  let anon_fun n = () in
  let speclist = [("-f", Arg.Set_string filename, "<filename>  Set input file name (If not specified, read from stdin)")] in
  Arg.parse speclist anon_fun "Usage:";
  if !filename <> "" then
    let fin = open_in !filename in
    let rec loop acc =
      match input_line fin with
      | l -> loop (l :: acc)
      | exception End_of_file -> close_in fin; List.rev acc
    in
    loop []
  else
    let rec loop acc =
      match read_line () with
      | l -> loop (l :: acc)
      | exception End_of_file -> List.rev acc
    in
    loop []

let cnvt_data data =
  List.map (fun l -> Str.split (Str.regexp ",") l) data
  |> List.map (fun l -> Array.of_list (List.map (int_of_string) l))
  |> List.to_seq
  |> Array.of_seq

let make_nbrtbl m_arr =
  let x_size = Array.length m_arr in
  let y_size = Array.length m_arr.(0) in
  let tbl = Hashtbl.create (x_size * y_size) in
  for i = 0 to x_size - 1 do
    for j = 0 to y_size - 1 do
      Hashtbl.add tbl (i, j)
        ([(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)]
         |> List.filter (fun (x, _) -> x >= 0 && x < x_size)
         |> List.filter (fun (_, y) -> y >= 0 && y < y_size))
    done
  done;
  tbl

let make_dist_arr m_arr =
  let x_size = Array.length m_arr in
  let y_size = Array.length m_arr.(0) in
  let dist_arr = Array.make_matrix x_size y_size max_int in
  dist_arr.(0).(0) <- m_arr.(0).(0);
  dist_arr

module Q = Make_bintree(struct
               type t = int * (int * int)
               let compare (x, (_, _)) (y, (_, _)) = x - y
             end)

let solve () =
  let matrix = read_data() |> cnvt_data in
  let nbr_tbl = make_nbrtbl matrix in
  let dist_arr = make_dist_arr matrix in
  let answer m_arr =
    let x_size = Array.length m_arr in
    let y_size = Array.length m_arr.(0) in
    m_arr.(x_size - 1).(y_size - 1)
  in    
  let pq = Q.create() in
  let pq = Q.insert pq (matrix.(0).(0), (0, 0)) in
  let rec loop pq =
    if Q.is_empty pq = true then
      answer dist_arr
    else
      let d, (x, y) = Q.find_min pq in
      let nbrs = Hashtbl.find nbr_tbl (x, y) in
      let rec loop' pq lst =
        match lst with
        | [] -> pq
        | (x, y) :: tl ->
           let new_d = d + matrix.(x).(y) in
           if new_d < dist_arr.(x).(y) then (
             dist_arr.(x).(y) <- new_d;
             loop' (Q.insert pq (new_d, (x, y))) tl
           ) else
             loop' pq tl
      in
      loop (loop' (Q.delete_min pq) nbrs)
  in
  loop pq

let () =
  Printf.printf "Answer: %d\n" (solve ())
