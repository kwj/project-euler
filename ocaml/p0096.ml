(* Project Euler: Problem 96 *)

(*
  [Grid]
      C0 C1 C2 C3 C4 C5 C6 C7 C8
     +--------+--------+--------+    R: Row      Cell(x,y) = RxCy
   R0|        |        |        |    C: Column    position = 9x + y
   R1|   B0   |   B1   |   B2   |    B: Box
   R2|        |        |        |
     +--------+--------+--------+    H: House
   R3|        |        |        |       H0..H8:  R0..R8
   R4|   B3   |   B4   |   B5   |      H9..H17:  C0..C8
   R5|        |        |        |     H18..H26:  B0..B8
     +--------+--------+--------+
   R6|        |        |        |    F: Floor        T: Tower
   R7|   B6   |   B7   |   B8   |      F0: B0,B1,B2    T0: B0,B3,B6
   R8|        |        |        |      F1: B3,B4,B5    T1: B1,B4,B7
     +--------+--------+--------+      F2: B6,B7,B8    T2: B2,B5,B8

                                     (note: I didn't use Floor and Tower)

  I added basic strategies until problems were solved. In other words,
  it's not generic program at all. It was fun, but exhausting.
  I think using backtracking was easier to solve :). 

   PS: I worte an another program that uses backtracking. Pls see 'p0096_alt.ml'.

  You will need the following files to run this program.
    - https://projecteuler.net/project/resources/p096_sudoku.txt
 *)

(* ---------------------------------------------------------------- *)

module Sudoku : sig
  type t
  val init : string -> t
  val get_size : t -> int
  val export : t -> string
  val display : t -> unit
  val dump : t -> string
  val restore : t -> string -> unit
  val digest : t -> Digest.t
  val naked_single : t -> unit
  val hidden_single : t -> unit
  val naked_pair : t -> unit
  val pointing_pair : t -> unit
  val hidden_pair : t -> unit
  val x_wing : t -> unit
  val is_complete : t -> bool
end = struct
  type cell = Fixed of int list | Cand of int list

  let rec uniq = function
    | [] -> []
    | hd1 :: (hd2 :: _ as tl) when hd1 = hd2 -> uniq tl
    | hd :: tl -> hd :: uniq tl

  let rec iota ?(start=0) ?(step=1) n =
    if n <= 0 then [] else start :: iota ~start:(start+step) ~step:step (pred n)

  let peel = function
    | Fixed l -> l
    | Cand l -> l
    
  class grid given =
    object(self)
      val mutable work_arr = Array.make 0 (Fixed [0])

      (* misc. *)
      method peek pos = work_arr.(pos)
      method poke pos c = work_arr.(pos) <- c

      (* show work array *)
      method display =
        let cell_nums c =
          List.length (peel c)
        in
        let width =  List.hd (List.rev (List.sort compare (List.map cell_nums (Array.to_list work_arr)))) in
        let nlist_format lst =
          let rec aux l acc =
            match l with
            | [] -> acc
            | hd :: tl ->
               aux tl (acc * 10 + hd)
          in
          let nstr = string_of_int (aux lst 0) in
          let slen = String.length nstr in
          let wlen = width - slen in
          let pad = if width = slen then 0 else wlen mod 2 in
          Printf.sprintf "%*s%s%*s" (wlen / 2 + pad) "" nstr (wlen / 2) ""
        in
        let line_format pos =
          let s1,s2,s3,s4,s5,s6,s7,s8,s9 =
            nlist_format(peel(work_arr.(pos  ))), nlist_format(peel(work_arr.(pos+1))),
            nlist_format(peel(work_arr.(pos+2))), nlist_format(peel(work_arr.(pos+3))),
            nlist_format(peel(work_arr.(pos+4))), nlist_format(peel(work_arr.(pos+5))),
            nlist_format(peel(work_arr.(pos+6))), nlist_format(peel(work_arr.(pos+7))),
            nlist_format(peel(work_arr.(pos+8)))
          in
          Printf.sprintf "%s %s %s | %s %s %s | %s %s %s" s1 s2 s3 s4 s5 s6 s7 s8 s9
        in
        let sep_line width =
          let rec repstr n s acc =
            if n = 0 then acc else repstr (pred n) s (acc ^ s)
          in
          (repstr (width * 3 + 3) "-" "") ^ "+" ^ (repstr (width * 3 + 4) "-" "") ^ "+" ^ (repstr (width * 3 + 3) "-" "")
        in            
        for i = 0 to 8 do
          if i = 3 || i = 6 then (
            Printf.printf "%s\n" (sep_line width)
          ) else (
            ()
          );
          Printf.printf "%s\n" (line_format (i * 9))
        done

      method dump =
        let rec aux l acc =
          match l with
          | [] -> acc
          | hd :: tl ->
             aux tl (acc * 10 + hd)
        in
        let cell_to_str = function
          | Fixed l -> string_of_int (aux l 0)
          | Cand l -> "#" ^ (string_of_int (aux l 0))
        in
        String.concat ";" (Array.to_list (Array.map cell_to_str work_arr))

      method restore data =
        let lst = Str.split (Str.regexp ";") data in
        if List.length lst <> Array.length work_arr then
          raise (Failure "invalid dump data")
        else
          work_arr <- List.map (fun s ->
                          if s.[0] <> '#' then
                            Fixed [int_of_string s]
                          else
                            Cand (List.map int_of_string (List.tl (Str.split (Str.regexp "") s)))) lst
                      |> Array.of_list
        
      (* get house numbers that the cell belongs to *)
      method get_houses pos =
        let div27, div9, mod9 = pos / 27, pos / 9, pos mod 9 in
        let row = div9 in
        let col = mod9 in
        let box = div27 * 3 + mod9 / 3 in
        [row; col + 9; box + 18]

      (* get cell positionss in the house *)
      method get_pos_in_house n =
        match n with
        | n when n < 9 -> iota 9 ~start:(n * 9)
        | n when n < 18 -> iota 9 ~start:(n - 9) ~step:9
        | n -> let x = ((n - 18) / 3) * 27 + ((n - 18) mod 3) * 3 in
               List.flatten ((iota 3 ~start:x) :: (iota 3 ~start:(x + 9)) :: (iota 3 ~start:(x + 18)) :: [])

      (* get cells in the house *)
      method get_cells_in_house n =
        let rec loop lst acc =
          match lst with
          | [] -> acc
          | hd :: tl -> loop tl ((hd, work_arr.(hd)) :: acc)
        in
        loop (List.rev (self#get_pos_in_house n)) []

      (* get cands in the house *)
      method get_cands_in_house n =
        let _, cell_lst = self#get_cells_in_house n |> List.split in
        List.filter (fun c ->
            match c with
            | Fixed _ -> false
            | Cand _ -> true) cell_lst
        |> List.map (fun c -> peel c)
        |> List.flatten
        |> List.sort compare
        |> uniq

      (* get cands in the houses which contain the cell *)
      method get_cands pos =
        let rec loop lst acc =
          match lst with
          | [] -> List.flatten acc |> List.sort compare |> uniq
          | hd :: tl -> loop tl ((self#get_cands_in_house hd) :: acc)
        in
        loop (self#get_houses pos) []

      (* remove candidate number from cells in the house [ex_pos is a list which specifies excluding cells] *)
      method remove_cand_from_house cand ex_pos house =
        let rec exclude tgts exs =
          match exs with
          | [] -> tgts
          | hd :: tl -> exclude (List.filter ((<>) hd) tgts) tl
        in
        let rec remove_cand lst =
          match lst with
          | [] -> ()
          | hd :: tl ->
             match work_arr.(hd) with
             | Fixed _ -> remove_cand tl
             | Cand l -> work_arr.(hd) <- Cand (List.filter ((<>) cand) l);
                         remove_cand tl
        in
        remove_cand (exclude (self#get_pos_in_house house) ex_pos)

      method len = Array.length work_arr

      method import s =
        let get_cands pos =
          let rec get_cells lst acc =
            match lst with
            | [] -> List.flatten acc |> List.sort compare |> uniq
            | hd :: tl -> get_cells tl ((self#get_pos_in_house hd) :: acc)
          in
          let rec final_nums lst acc =
            match lst with
            | [] -> List.flatten acc |> List.sort compare |> uniq
            | hd :: tl ->
               match work_arr.(hd) with
               | Cand _ -> final_nums tl acc
               | Fixed l -> final_nums tl (l :: acc)
          in
          let rec loop cands exs =
            match exs with
            | [] -> Cand cands
            | hd :: tl -> loop (List.filter ((<>) hd) cands) tl
          in
          loop (iota 9 ~start:1) (final_nums (get_cells (self#get_houses pos) []) [])
        in
        work_arr <- Array.of_list (List.map
                                     (fun c -> if c = "0" then Cand (iota 9 ~start:1) else Fixed [int_of_string c])
                                     (Str.split (Str.regexp "") s));
        Array.iteri
          (fun i elt -> match elt with
                        | Cand _ -> work_arr.(i) <- get_cands i
                        | Fixed _ -> ())
          work_arr

      method export =
        let cand_to_numstr e =
          match e with
          | Fixed l -> string_of_int (List.hd l)
          | Cand _ -> "0"
        in
        Array.fold_left (fun s elt -> s ^ (cand_to_numstr elt)) "" work_arr

      initializer
        begin
          self#import given
        end
    end

  type t = grid

  let init s = new grid s

  let get_size g = g#len

  let export g = g#export

  let dump g = g#dump

  let restore g s = g#restore s
             
  let peek g pos = g#peek pos

  let digest g = Digest.string g#dump
    
  let display g = g#display
    
  let rec naked_single g =
    let prev_digest = digest g in
    for i = 0 to (get_size g) - 1 do
      match g#peek i with
      | Fixed _ -> ()
      | Cand l ->
         if List.length l > 1 then
           ()
         else (
           g#poke i (Fixed l);
           let rec loop = function
             | [] -> ()
             | hd :: tl ->
                g#remove_cand_from_house (List.hd l) [i] hd;
                loop tl
           in
           loop (g#get_houses i)
         )
    done;
    if prev_digest <> digest g then
      naked_single g
    else
      ()

  let cand_cells g house =
    g#get_cells_in_house house
    |> List.filter
         (fun (p, c) -> match c with | Fixed _ -> false | Cand _ -> true)
    |> List.map (fun (p, c) -> (p, (peel c)))

  let rec hidden_single g =
    let prev_digest = digest g in
    let rec loop lst =
      match lst with
      | [] -> ()
      | hd :: tl ->
         let cells = cand_cells g hd in
         let find_hidden_single cands_lst =
           let rec aux lst prev =
             match lst with
             | [] -> None
             | a :: [] when a <> prev -> Some a
             | a :: b :: _ when a <> prev && a <> b -> Some a
             | a :: b -> aux b a
           in
           aux cands_lst 0
         in
         match find_hidden_single (List.map (fun (_, l) -> l) cells
                                   |> List.flatten
                                   |> List.sort compare) with
         | None -> loop tl
         | Some v ->
            let pos, cands = List.hd (List.filter (fun (p, l) -> List.mem v l) cells) in
            g#poke pos (Fixed [v]);
            let rec loop_rm = function
              | [] -> ()
              | x :: xs ->
                 g#remove_cand_from_house v [pos] x;
                 loop_rm xs
            in
            loop_rm (g#get_houses pos);
            loop tl
    in
    loop (iota 27);
    if prev_digest <> digest g then (
      naked_single g;
      hidden_single g
    ) else
      ()

  let naked_pair g =
    let rec loop = function
      | [] -> ()
      | house :: tl ->
         let cells = cand_cells g house in
         let rec aux = function
           | [] -> ()
           | (p1, l1) :: ((p2, l2) :: _ as tl) when l1 = l2 ->
              (
                (* eliminate other candidates from the house *)
                g#remove_cand_from_house (List.nth l1 0) [p1; p2] house;
                g#remove_cand_from_house (List.nth l1 1) [p1; p2] house;
                let h1, h2 = g#get_houses p1, g#get_houses p2 in
                match house with
                (* row or col *)
                | n when n < 18 ->
                   if List.nth h1 2 = List.nth h2 2 then (
                     (* this pair is in same box *)
                     g#remove_cand_from_house (List.nth l1 0) [p1; p2] (List.nth h1 2);
                     g#remove_cand_from_house (List.nth l1 1) [p1; p2] (List.nth h1 2)
                   )
                (* box *)
                | _ ->
                   if List.nth h1 0 = List.nth h2 0 then (
                     (* this pair is in same raw *)
                     g#remove_cand_from_house (List.nth l1 0) [p1; p2] (List.nth h1 0);
                     g#remove_cand_from_house (List.nth l1 1) [p1; p2] (List.nth h1 0)
                   ) else
                     if List.nth h1 1 = List.nth h2 1 then (
                       (* this pair is in same col *)
                       g#remove_cand_from_house (List.nth l1 0) [p1; p2] (List.nth h1 1);
                       g#remove_cand_from_house (List.nth l1 1) [p1; p2] (List.nth h1 1)
                     )
              );
              aux tl
           | (p1, l1) :: tl -> aux tl
         in
         aux (List.sort
                (fun (_, l1) (_, l2) -> List.compare (fun a1 a2 -> a1 - a2) l1 l2)
                (List.filter (fun (p, l) -> List.length l = 2) cells));
         loop tl
    in
    loop (iota 27)

  let pointing_pair g =
    let rec loop = function
      | [] -> ()
      | house :: tl ->
         let cells = cand_cells g house in
         let rec find_pair lst acc result =
           match lst with
           | [] -> List.rev result |> List.filter (fun (elt, lst) -> List.length lst = 2)
           | (e1, p1) :: ((e2, p2) :: _ as tl) when e1 = e2 -> find_pair tl (p1 :: acc) result
           | (e1, p1) :: tl -> find_pair tl [] ((e1, (p1 :: acc)) :: result)
         in
         let pair_lst = find_pair (List.map (fun (p, lst) -> List.map (fun elt -> (elt, p)) lst) cells
                                   |> List.flatten
                                   |> List.sort compare) [] [] in
         let rec aux = function
           | [] -> ()
           | (elt, [p1; p2]) :: p_tl ->
              let h1, h2 = g#get_houses p1, g#get_houses p2 in
              (match house with
              (* row or col *)
              | n when n < 18 ->
                 if List.nth h1 2 = List.nth h2 2 then (
                   (* this pair is in same box *)
                   g#remove_cand_from_house elt [p1; p2] (List.nth h1 2)
                 )
              (* box *)
              | _ ->
                 if List.nth h1 0 = List.nth h2 0 then
                   (* this pair is in same raw *)
                   g#remove_cand_from_house elt [p1; p2] (List.nth h1 0)
                 else
                   if List.nth h1 1 = List.nth h2 1 then
                     (* this pair is in same col *)
                     g#remove_cand_from_house elt [p1; p2] (List.nth h1 1)
              );
              aux p_tl
           | _ -> assert false
         in
         aux pair_lst;
         loop tl
    in
    loop (iota 27)

  let hidden_pair g =
    let rec loop = function
      | [] -> ()
      | house :: tl ->
         let cells = cand_cells g house in
         let rec find_pair lst acc result =
           match lst with
           | [] -> List.rev result |> List.filter (fun (elt, lst) -> List.length lst = 2)
           | (e1, p1) :: ((e2, p2) :: _ as tl) when e1 = e2 -> find_pair tl (p1 :: acc) result
           | (e1, p1) :: tl -> find_pair tl [] ((e1, List.sort compare (p1 :: acc)) :: result)
         in
         let pair_lst = find_pair (
                            find_pair (List.map (fun (p, lst) -> List.map (fun elt -> (elt, p)) lst) cells
                                       |> List.flatten
                                       |> List.sort compare) [] []
                            |> List.map (fun (lst, p) -> (p, lst))) [] []
         in
         let rec aux = function
           | [] -> ()
           | ([p1; p2], c_lst) :: tl ->
              g#poke p1 (Cand c_lst);
              g#poke p2 (Cand c_lst);
              aux tl
           | _ -> assert false
         in
         aux pair_lst;
         loop tl
    in
    loop (iota 27)

  let x_wing g =
    let is_x pos cands =
      let houses = g#get_houses pos in
      let house_r, house_c = List.nth houses 0, List.nth houses 1 in
      let check_rect cand r_lst c_lst =
        let rec loop_r r_l (acc: (int * int list) list) =
          match r_l with
          | [] -> acc
          | hd_r :: tl ->
             let diff_r = hd_r - pos in
             let rec loop_c c_l acc =
               match c_l with
               | [] -> acc
               | hd_c :: tl ->
                  match g#peek (hd_c + diff_r) with
                  | Fixed _ -> loop_c tl acc
                  | Cand l ->
                     if List.mem cand l = false then
                       loop_c tl acc
                     else
                       let p1_houses = g#get_houses pos in
                       let p4_houses = g#get_houses (hd_c + diff_r) in
                       let house_1r, house_1c = List.nth p1_houses 0, List.nth p1_houses 1 in
                       let house_4r, house_4c = List.nth p4_houses 0, List.nth p4_houses 1 in
                       let cells_1r = cand_cells g house_1r |> List.filter (fun (_, l) -> List.mem cand l) in
                       let cells_1c = cand_cells g house_1c |> List.filter (fun (_, l) -> List.mem cand l) in
                       let cells_4r = cand_cells g house_4r |> List.filter (fun (_, l) -> List.mem cand l) in
                       let cells_4c = cand_cells g house_4c |> List.filter (fun (_, l) -> List.mem cand l) in
                       if (List.length cells_1r = 2 && List.length cells_4r = 2) || (List.length cells_1c = 2 && List.length cells_4c = 2) then
                         loop_c tl ((cand, [pos; hd_r; hd_c; hd_c + diff_r]) :: acc)
                       else
                         loop_c tl acc
             in
             loop_r tl (loop_c c_lst acc)
        in
        loop_r r_lst []
      in
      let rec cands_loop cands_lst result =
        match cands_lst with
        | [] -> List.flatten result
        | cand :: tl ->
           let cells_r = cand_cells g house_r
                         |> List.filter (fun (p, l) -> p > pos)
                         |> List.sort (fun (p1, _) (p2, _) -> p2 - p1)
                         |> List.filter (fun (_, l) -> List.mem cand l)
                         |> List.map (fun (p, _) -> p) in
           let cells_c = cand_cells g house_c
                         |> List.filter (fun (p, _) -> p > pos)
                         |> List.sort (fun (p1, _) (p2, _) -> p2 - p1)
                         |> List.filter (fun (_, l) -> List.mem cand l)
                         |> List.map (fun (p, _) -> p) in
           let tmp = check_rect cand cells_r cells_c in
           
           if tmp = [] || List.length tmp <> 1 then
             cands_loop tl result
           else
             cands_loop tl (tmp :: result)
      in
      cands_loop cands []
    in
             
    let remove_cands_from_side lst =
      let rec loop = function
        | [] -> ()
        | (cand, l) :: tl ->
           (*
             p1 -- p2
              |     |
             p3 -- p4
            *)
           let p1, p2, p3, p4 = List.nth l 0, List.nth l 1, List.nth l 2, List.nth l 3 in
           let p1_house, p4_house = g#get_houses p1, g#get_houses p4 in
           g#remove_cand_from_house cand [p1; p2] (List.nth p1_house 0);
           g#remove_cand_from_house cand [p1; p3] (List.nth p1_house 1);
           g#remove_cand_from_house cand [p3; p4] (List.nth p4_house 0);
           g#remove_cand_from_house cand [p2; p4] (List.nth p4_house 1);
           loop tl
      in
      loop lst
    in
        
    for i = 0 to (get_size g) - 1 do
      match g#peek i with
      | Fixed _ -> ()
      | Cand l ->
         let result = is_x i l in
         if List.length result > 0 then (
           remove_cands_from_side result
         )
    done

  let is_complete g =
    match String.index_from_opt g#export 0 '0' with
    | None -> true
    | Some _ -> false

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

let clunky_solver s =
  let g = Sudoku.init s in
  Sudoku.naked_single g;
  Sudoku.hidden_single g;
  let prev_digest = ref (Digest.string "dummy") in
  while Sudoku.is_complete g = false && Digest.compare !prev_digest (Sudoku.digest g) <> 0 do
    prev_digest := Sudoku.digest g;
    Sudoku.pointing_pair g;
    Sudoku.naked_pair g;
    Sudoku.hidden_pair g;
    Sudoku.x_wing g;
    Sudoku.naked_single g;
    Sudoku.hidden_single g
  done;
  Sudoku.display g;
  Sudoku.export g
  
let solve () =
  let givens = read_data() |> cnvt_data in
  let rec loop q i acc =
    match q with
    | [] -> acc
    | hd :: tl ->
       Printf.printf "\n[%d]\n" i;
       print_endline hd;
       loop tl (succ i) (acc + int_of_string (String.sub (clunky_solver hd) 0 3))
  in
  loop givens 1 0
  
let () =
  Printf.printf "Answer: %d\n" (solve ())

