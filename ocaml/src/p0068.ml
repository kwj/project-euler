(* Project Euler: Problem 68 *)

(*
 * ring: int array [size: n_gon * 2 + 1]
 *
 *         r1
 *           \
 *           r0  [r6 <- r0]
 *          /  \
 *        r4---r2-r3
 *        /
 *      r5
 *
 *         r1
 *           \  [r10 <- r0]
 *           r0  r3
 *          /  \ /
 *        r8   r2
 *        /\   /
 *     r9 r6-r4-r5
 *           \
 *            r7
 *
 * bit_mask: 1: used number, 0: unused number
 *   the following is an example when n_gon = 5.
 *     0x11111111110
 *       ^        ^
 *       10  ...  1
 *)

open Core

let is_valid x y bit_mask n_gon =
  0 < x
  && x <= n_gon * 2
  && 0 < y
  && y <= n_gon * 2
  && x <> y
  && (1 lsl x) lor (1 lsl y) land bit_mask = 0
;;

let rec dfs n_gon idx bit_mask ring total result =
  let start_pos = 1 in
  if idx = (n_gon * 2) - 2
  then (
    let tmp = total - ring.(0) - ring.(idx) in
    if
      (0 < tmp && tmp <= n_gon * 2)
      && tmp > ring.(start_pos)
      && (1 lsl tmp) land bit_mask = 0
    then (
      ring.(idx + 1) <- tmp;
      let rec aux s idx =
        let s = Printf.sprintf "%d%d%d%s" ring.(idx + 1) ring.(idx) ring.(idx + 2) s in
        if idx = 0 then s else aux s (idx - 2)
      in
      result := aux "" idx :: !result))
  else (
    let rec loop = function
      | [] -> ()
      | external_node :: xs ->
        let internal_node = total - ring.(idx) - external_node in
        if is_valid external_node internal_node bit_mask n_gon
        then (
          ring.(idx + 1) <- external_node;
          ring.(idx + 2) <- internal_node;

          (* pruning: if starting node is the smallest external node, continue to search *)
          if ring.(start_pos) <= external_node
          then
            dfs
              n_gon
              (idx + 2)
              ((1 lsl external_node) lor (1 lsl internal_node) lor bit_mask)
              ring
              total
              result);
        loop xs
    in
    loop (List.range 1 (n_gon * 2) ~stop:`inclusive))
;;

let compute n_gon =
  let ring = Array.init ((n_gon * 2) + 1) ~f:(fun _ -> 0) in
  let result = ref [] in

  (* The minimum total of the line which contains 'n_gon * 2' is 1 + 2 + (n_gon * 2) = n_gon * 2 + 3. *)
  (* The maximum total of the line which contains '1' is 1 + (n_gon * 2 - 1) + (n_gon * 2) = n_gon * 4. *)
  for total = (n_gon * 2) + 3 to n_gon * 4 do
    for n = 1 to n_gon * 2 do
      ring.(0) <- n;
      ring.(n_gon * 2) <- n;
      dfs n_gon 0 (1 lsl n) ring total result
    done
  done;

  (* Only 16-digit strings are covered in this problem (when n_gon == 5). *)
  if n_gon = 5 then result := List.filter ~f:(fun s -> String.length s = 16) !result;

  List.map ~f:(fun s -> Int.of_string s) !result
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let solve () = compute 5 |> Int.to_string

(* Test *)

let%test_unit "3" = [%test_eq: int] (compute 3) 432621513
let%test_unit "5" = [%test_eq: int] (compute 5) 6531031914842725
