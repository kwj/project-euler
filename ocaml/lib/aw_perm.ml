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

let op_sigma arr =
  let len = Array.length arr in
  let tmp = arr.(0) in
  Array.blit arr 1 arr 0 (len - 1);
  arr.(len - 1) <- tmp

let op_tau arr =
  let tmp = arr.(0) in
  arr.(0) <- arr.(1); arr.(1) <- tmp

let is_tau arr t_arr =
  let len = Array.length arr in
  let find_r arr =
    let rec aux i =
      if i < 0 then failwith "index error"
      else
        if arr.(i) = len - 1 then
          match i with
          | 0 -> 2
          | _ -> (i + 1) mod len
        else
          aux (pred i)
    in
    aux (len - 1)
  in
  if Array.for_all2 (=) arr t_arr then
    false
  else
    match arr.(find_r arr) with
    | r when r = len - 2 ->
       arr.(1) = 0
    | r ->
       arr.(1) = r + 1

let constr_lst arr lst =
  let rec reorder l acc =
    match l with
    | [] -> List.rev acc
    | hd :: tl -> reorder tl ((List.nth lst hd) :: acc)
  in
  reorder (Array.to_list arr) []

let generator lst =
  let len = List.length lst in
  match len with
  | n when n <= 0 -> failwith "size error"
  | 1 ->
     let cnt = ref 1 in
     let next () =
       if !cnt = 0 then None
       else (
         cnt := !cnt - 1;
         Some lst
       )
     in
     next
  | 2 ->
     let cnt = ref 2 in
     let l = ref [[(List.nth lst 0); (List.nth lst 1)]; [(List.nth lst 1); (List.nth lst 0)]] in
     let next () =
       if !cnt = 0 then None
       else (
         cnt := !cnt - 1;
         Some (List.nth !l !cnt)
       )
     in
     next
  | _ ->
     let trigger = Array.init len (fun i -> len - 1 - i) in
     let a_idx = Array.copy trigger in
     let a_idx_end = Array.copy trigger in
     let end_flag = ref false in
     op_tau a_idx;
     op_sigma a_idx_end;
     op_tau a_idx_end;
     let next () =
       if !end_flag = true then
         None
       else
         let result = constr_lst a_idx lst in
         if Array.for_all2 (=) a_idx a_idx_end then (
           end_flag := true
         ) else (
           if is_tau a_idx trigger then op_tau a_idx else op_sigma a_idx;
         );
         Some result
     in
     next
