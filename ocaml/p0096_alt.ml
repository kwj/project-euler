(* Project Euler: Problem 96 [another version] *)

(*
  [Grid]
      C0 C1 C2 C3 C4 C5 C6 C7 C8
     +--------+--------+--------+    R: Row
   R0|        |        |        |    C: Column
   R1|   B0   |   B1   |   B2   |    B: Box
   R2|        |        |        |
     +--------+--------+--------+    Cell(x,y) = RxCy  
   R3|        |        |        |      position = 9x + y
   R4|   B3   |   B4   |   B5   |
   R5|        |        |        |
     +--------+--------+--------+
   R6|        |        |        |
   R7|   B6   |   B7   |   B8   |
   R8|        |        |        |
     +--------+--------+--------+

  I use the following Algorithm X library in this source file.
    algo_x (https://github.com/kwj/algo_x)  [(c) 2021 Jun Kawai]

  [Matrix]
         R0C0 R0C1 .. R8C8 | R0#1 R0#2 .. R8#8 R8#9 | C0#1 C0#2 .. C8#8 C8#9 | B0#1 B0#2 .. B8#8 B8#9
  ----------------------------------------------------------------------------------------------------
  R0C0#1   1    0  ..   0      1    0  ..   0    0      1    0  ..   0    0      1    0  ..   0    0
  R0C0#2   1    0  ..   0      0    1  ..   0    0      0    1  ..   0    0      0    1  ..   0    0
   ...
  R0C0#9   1    0  ..   0      0    0  ..   0    0      0    0  ..   0    0      0    0  ..   0    0
  R0C1#1   0    1  ..   0      1    0  ..   0    0      0    0  ..   0    0      1    0  ..   0    0
   ...
  R0C8#9   0    0  ..   0      0    0  ..   0    0      0    0  ..   0    1      0    0  ..   0    0
  R1C0#1   0    0  ..   0      0    0  ..   0    0      1    0  ..   0    0      0    0  ..   0    0
   ...
  R8C8#8   0    0  ..   1      0    0  ..   1    0      0    0  ..   1    0      0    0  ..   1    0
  R8C8#9   0    0  ..   1      0    0  ..   0    1      0    0  ..   0    1      0    0  ..   0    1

    Row: RxCy#N -> Cell(x,y) = N
    Col: RxCy -> some number is in Cell(x,y)       [cell constraint] (81 columns)
         Rx#N -> number 'N' is in the row Rx       [row constraint] (81 columns)
         Cy#N -> number 'N' is in the column Cy    [column constraint] (81 columns)
         Bz#N -> number 'N' is in the box Bz       [box constraint] (81 columns)

  Algorithm X (using Dancing Links)
    https://en.wikipedia.org/wiki/Algorithm_X
    https://en.wikipedia.org/wiki/Dancing_Links

  You will need the following files to run this program.
    - https://projecteuler.net/project/resources/p096_sudoku.txt
 *)

(* ---------------------------------------------------------------- *)

(* Algorithm X *)
type node = {
    mutable tag : string;
    mutable cnt : int;
    mutable l : node;
    mutable r : node;
    mutable u : node;
    mutable d : node;
    mutable c : node
}

type t = {
    head : node;
    cs : node array;
    ncol : int;
    mutable nrow : int;
    mutable ans : string list list
}
       
let _dlx_node () =
  let rec n = { tag = "head"; cnt = 0; l = n; r = n; u = n; d = n; c = n} in
  n

(*
  head -->
         |
    +----.--------+---------+---------+--     -+---------+
  <--> head_node <-> col_1 <-> col_2 <-> .... <-> col_n <-->
    +-------------+---------+---------+--     -+---------+
  cs     .(0)         .(1)      .(2)               .(n)

  ncol = n
 *)
  
let dlx_init col_size =
  if col_size <= 0 then
    raise (Failure "invalid column size")
  else
    let rec loop n prev acc =
      if n = 0 then
        acc
      else
        let cell = _dlx_node () in
        cell.r <- prev;
        prev.l <- cell;
        cell.tag <- "col_" ^ (string_of_int n);
        loop (pred n) cell (cell :: acc)
    in
    (*
      I think it would be safer to allocate array first and then create nodes instead of
      creating array from nodes list. I feel like it depends on the implementation.
      Howerever, so far, this has not been a problem.
     *)
    let head_node = _dlx_node () in
    let col_arr = Array.of_list (head_node :: (loop col_size head_node [])) in
    col_arr.(1).l <- head_node;
    head_node.r <- col_arr.(1);
    head_node.cnt <- max_int;

    {head = head_node; cs = col_arr; ncol = col_size; nrow = 0; ans = []}

(*
           :          :         :                   :
           |          |         |                   |
          u|         u|        u|                  u|
    +-----------r-+-------r-+-------r-+--     -+-------r-+  tag: "haed", "col_#"
 <---> head_node <-> col_1 <-> col_2 <-> .... <-> col_n <---> 
    +-l-----------+-l-------+-l-------+--     -+-l-------+
           |d         |d        |d                  |d
           |          |         |                   |
           |         u| r      u| r                 |       tag: "1" or specified value
  <----------------> node <--> node <----....-------------->
           |        l |d      l |d                  |
           |          |         |                   |
           |          |        u| r                u| r     tag: "2" or specified value
  <--------------------------> node <----....----> node <-->
           |          |       l |d                l |d
           :          :         :                   :

               [c]ol_n
                 |                 l : node
            [u]p |                 r : node
               | |                 u : node
   [l]eft <-- node --> [r]ight     d : node
               |                   c : node
             [d]own
 *)
    
let dlx_add_row ?name dlx idx_lst =
  let s_lst = List.sort compare idx_lst in
  if List.hd s_lst < 1 || List.nth s_lst (List.length s_lst - 1) > dlx.ncol then
    raise (Failure "invalid index value")
  else
    dlx.nrow <- dlx.nrow + 1;
    let name = match name with
      | None -> string_of_int dlx.nrow
      | Some s -> s
    in
    let rec hook_node first prev lst =
      match lst with
      | [] -> first, prev
      | idx :: tl ->
         let cell = _dlx_node () in
         cell.tag <- name;
         cell.u <- dlx.cs.(idx).u;
         cell.u.d <- cell;
         cell.d <- dlx.cs.(idx);
         cell.d.u <- cell;
         cell.c <- dlx.cs.(idx);
         dlx.cs.(idx).cnt <- dlx.cs.(idx).cnt + 1;
         if prev <> [] then
           let p_cell = List.hd prev in
           p_cell.r <- cell;
           cell.l <- p_cell;
           hook_node first [cell] tl
         else
           hook_node [cell] [cell] tl
    in
    let first, last = hook_node [] [] s_lst in
    if first <> [] then (
      let f_cell, l_cell  = List.hd first, List.hd last in
      l_cell.r <- f_cell;
      f_cell.l <- l_cell
    )

type direction = Left | Right | Up | Down

let _follow n f dir =
  let stop = n in
  let next n =
    match dir with | Left -> n.l | Right -> n.r | Up -> n.u | Down -> n.d
  in
  let rec loop n =
    if n != stop then (
      f n;
      loop (next n)
    )
  in
  loop (next n)
                                   
let _dlx_cover col_n =
  let _cover n =
    n.u.d <- n.d;
    n.d.u <- n.u;
    n.c.cnt <- n.c.cnt - 1
  in
  let _cover_row n =
    _follow n _cover Right
  in
  col_n.r.l <- col_n.l;
  col_n.l.r <- col_n.r;
  _follow col_n _cover_row Down
        
let _dlx_uncover col_n =
  let _uncover n =
    n.u.d <- n;
    n.d.u <- n;
    n.c.cnt <- n.c.cnt + 1
  in
  let _uncover_row n =
    _follow n _uncover Left
  in
  _follow col_n _uncover_row Up;
  col_n.r.l <- col_n;
  col_n.l.r <- col_n

let dlx_solve dlx =
  let rec find_min n stop result =
    if n == stop then
      result
    else
      if n.cnt < result.cnt then
        find_min n.r stop n
      else
        find_min n.r stop result
  in
  let rec solve d acc =
    if d.head.r == d.head then
      d.ans <- (List.rev acc) :: d.ans
    else (
      let col_n = find_min d.head.r d.head d.head.r in
      if col_n.cnt > 0 then (
        _dlx_cover col_n;
        let _iter n =
          _follow n (fun x -> _dlx_cover x.c) Right;
          solve d (n.tag :: acc);
          _follow n (fun x -> _dlx_uncover x.c) Left;
        in
        _follow col_n _iter Down;
        _dlx_uncover col_n
      )
    )
  in
  solve dlx [];
  match dlx.ans with
  | [] -> None
  | lst -> Some lst


(* ---------------------------------------------------------------- *)

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
  let trim s =
    Str.global_replace (Str.regexp "[^0-9.]") "" s
    |> Str.global_replace (Str.regexp "\\.") "0"
  in
  let rec loop lines acc result =
    match lines with
    | [] ->
       List.rev (List.map trim ((List.fold_left (^) "" (List.rev acc)) :: result))
    | hd :: tl ->
       if Str.string_match (Str.regexp "^[0-9.]") hd 0 then
         loop tl (hd :: acc) result
       else
         if (Str.string_match (Str.regexp "^-") hd 0) = true || acc = [] then
           loop tl acc result
         else
           loop tl [] ((List.fold_left (^) "" (List.rev acc)) :: result)
  in
  loop data [] []

let make_dlx q =
  let d = dlx_init 324 in      (* number of columns = 81 + 81 + 81 + 81 *)
  let add_row pos n =
    let r, c, b = pos / 9, pos mod 9, (pos / 27) * 3 + (pos mod 9) / 3 in
    let add_row' num =
      let tag = Printf.sprintf "R%dC%d#%d" r c num in
      dlx_add_row ~name:tag d [(pos + 1);
                               (81 + 1) + r * 9 + (num - 1);
                               (81 * 2 + 1) + c * 9 + (num - 1);
                               (81 * 3 + 1) + b * 9 + (num - 1)]
    in
    if n <> 0 then
      add_row' n
    else
      for i = 1 to 9 do
        add_row' i
      done
  in
  List.iteri (fun i n -> add_row i n) (List.map int_of_string (Str.split (Str.regexp "") q));
  d
  
let solve () =
  let givens = read_data() |> cnvt_data in
  let pick_num lst =
    let rec loop n l acc =
      if n = 0 then
        acc
      else
        match l with
        | [] -> assert false
        | hd :: tl ->
           loop (pred n) tl (acc * 10 + int_of_string (List.hd (List.rev (Str.split (Str.regexp "") hd))))
    in
    loop 3 lst 0
  in
  let rec loop q i acc =
    match q with
    | [] -> acc
    | hd :: tl ->
       let d = make_dlx hd in
       match dlx_solve d with
       | None -> loop tl (succ i) acc
       | Some l ->
          let ans = pick_num (List.sort compare (List.hd l)) in
          loop tl (succ i) (acc + ans)
  in
  loop givens 1 0
  
let () =
  Printf.printf "Answer: %d\n" (solve ())
