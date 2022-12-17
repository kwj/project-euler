(* Project Euler: Problem 88 *)

(*
  N(k) = a1 + a2 + ... + ak = a1 * a2 * ... * ak
  min_N(k): minimal product-sum N(k)

  when k = 2
    sum {2,2} = prod {2,2}
  when k > 2 and {a1, a2, a3, ..., ak}
    min sum = sum {1, 1, ..., 1} = k
    --> min_N(k) >= k
  when k > 2 and {a1, a2, a3, ..., ak} = {1, ..., 1, 2, k}
    for all k>2, there exists Ak = {a1, a2, ..., ak} = {1, 1, ..., 1, 2, k}, prod Ak = sum Ak = N(k) = 2k
    --> min_N(k) <= 2k

  2 <= k <= 12000
  --> k <= N(k) <= 24000

    >>> math.log2(24000)
    14.550746785383243
    N(2) = {2, 2}
    N(k) = {a1, a2, ..., an, 1, 1, ..., 1}  [k>=3,n<k]
      2 <= n <= 14  [a1, ..., an > 1]

  I'll calculate product-sum numbers from prime factors.
 *)

(*
  example: start = 2, limit = 31

  v
  --------
  2,2
  2,2,2
  2,2,2,2
  2,2,2,3    // 2*2*2*2*2 = 32 > limit
  2,2,3      // 2*2*2*4 = 32 > limit
  2,2,4
  2,2,5
  2,2,6
  2,2,7
  2,3        // 2*2*8 = 32 > limit
  2,3,3
  2,3,4
  2,3,5
  2,4        // 2*3*6 = 36 > limit
  2,5
  2,6
  2,7
  2,8
  2,9
  2,10
  2,11
  2,12
  2,13
  2,14
  2,15
  3,3        // 2*16 = 32 > limit, 3 <= int(sqrt(limit)) = 5
  3,3,3
  3,4        // 3*3*4 = 36 > limit
  3,5
  3,6
  3,7
  3,8
  3,9
  3,10
  4,4        // 3*11 = 33 > limit, 4 <= int(sqrt(limit)) = 5
  4,5
  4,6
  4,7
  5,5        // 4*8 = 32 > limit, 5 <= int(sqrt(limit)) = 5
  5,6
  <end>      // 5*7 = 35 > limit, 6 > int(sqrt(limit)) = 5
 *)

open Core

let pf_generator limit =
  (* prime factors [reverse] : ak, ..., a2, a1 *)
  let pf_lst = ref [2; 2] in
  let prod_pf pf_lst =
    List.fold ~init:1 ~f:( * ) pf_lst
  in
  let create_tpl pf_lst =
    let prod = prod_pf pf_lst in
    let k = prod - (List.fold ~init:0 ~f:(fun acc n -> acc + (n - 1)) pf_lst) in
    Some (k, prod)
  in
  let next () =
    let lst = !pf_lst in
    if prod_pf lst > limit then (
      None
    ) else (
      if prod_pf (List.hd_exn lst :: lst) <= limit then (
        (* [f; ...; b; a] -> [f; f; ...; b; a] *)
        pf_lst := (List.hd_exn lst) :: lst
      ) else (
        if prod_pf ((List.hd_exn lst + 1) :: List.tl_exn lst) <= limit then
          (* [f; ...; b; a] -> [(f+1); ...; b; a] *)
          pf_lst := (List.hd_exn lst + 1) :: List.tl_exn lst
        else
          let tmp = ((List.nth_exn lst 1) + 1) :: List.tl_exn (List.tl_exn lst) in
          if List.length tmp = 1 then
            (* [b; a] -> [(a+1); (a+1)] *)
            pf_lst := (List.hd_exn tmp) :: tmp
          else
            (* [f; e; ...; b; a] -> [(e+1); ...; b; a] *)
            pf_lst := tmp
      );
      create_tpl lst
    )
  in
  next

let solve num =
  let pf_gen = pf_generator (num * 2) in
  let k_tbl = Array.create ~len:(num + 1) 0 in
  let rec loop () =
    match pf_gen () with
      None ->
        Array.to_list k_tbl |> List.dedup_and_sort ~compare |> List.fold ~init:0 ~f:(+)
    | Some (k, v) ->
        if k <= num && (k_tbl.(k) = 0 || v < k_tbl.(k)) then (
          k_tbl.(k) <- v
        );
        loop ()
  in
  loop ()

let exec () =
  Int.to_string (solve (12_000))

let () = Euler.Task.run exec
