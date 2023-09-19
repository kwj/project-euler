
(*
  Algorithm X (using Dancing Links)
    https://arxiv.org/abs/cs/0011047
    https://en.wikipedia.org/wiki/Algorithm_X
    https://en.wikipedia.org/wiki/Dancing_Links
 *)

(*
 MIT License

Copyright (c) 2021 Jun Kawai

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 *)

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
  type: t

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
      However, so far, this has not been a problem yet.
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
            [u]p :                 r : node
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

(* auxiliary functions *)

let _dlx_to_int dlx =
  List.map (fun l -> List.map (fun s -> int_of_string s) l) dlx.ans

let _dlx_print dlx =
  Printf.printf "[column chain]\n";
  let rec loop_1 n =
    if n != dlx.head then (
      Printf.printf "%s(%d) - " n.tag n.cnt;
      loop_1 n.r
    ) else
      Printf.printf ">>>>\n\n"
  in
  Printf.printf "%s - " dlx.head.tag;
  loop_1 dlx.head.r;

  Printf.printf "[node chain in column]\n";
  for i = 1 to Array.length dlx.cs - 1 do
    let rec loop_2 n =
      if n != dlx.cs.(i) then (
        Printf.printf "%s - " n.tag;
        loop_2 n.d
      ) else
        Printf.printf ">>\n"
    in
    Printf.printf "%s - " dlx.cs.(i).tag;
    loop_2 dlx.cs.(i).d
  done;

  print_newline ()
