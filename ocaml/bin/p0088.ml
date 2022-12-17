(* Project Euler: Problem 88 *)

(*
  when k = 2
    sum {2,2} = prod {2,2}
  when k > 2 and {a1, a2, a3, ..., ak}
    min sum = sum {1, 1, ..., 1} = k
    --> N(k) >= k
  when k > 2 and {a1, a2, a3, ..., ak} = {1, ..., 1, 2, k}
    prod {...} = sum {...} = 2k
    --> N(k) <= 2k

  2 <= k <= 12000
  --> k <= N(k) <= 24000

    >>> math.log2(24000)
    14.550746785383243
    N(2) = {2, 2}
    N(k) = {a1, a2, ..., an, 1, 1, ..., 1}  [k>=3,n<k]
      2 <= n <= 14  [a1, ..., an > 1]

  I'll calculate product-sum numbers from prime factors.
 *)

(* ---------------------------------------------------------------- *)

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

  pf_lst: prime factors (reverse)
 *)
let pf_generator limit =
  let sqr_ulimit = truncate @@ sqrt @@ float limit in
  let pf_lst = ref [2] in
  let prod_lst lst =
    List.fold_left ( * ) 1 lst
  in
  let create_tpl lst =
    let prod = prod_lst lst in
    let k = prod - (List.fold_left (fun m n -> m + (n - 1)) 0 lst) in
    Some (k, prod)
  in
  let next_pf () =
    if prod_lst (List.hd !pf_lst :: !pf_lst) <= limit then (
      (* [c; b; a] -> [c; c; b; a] *)
      pf_lst := List.hd !pf_lst :: !pf_lst;
      create_tpl !pf_lst
    ) else (
      if prod_lst ((List.hd !pf_lst + 1) :: List.tl !pf_lst) <= limit then (
        (* [c; b; a] -> [(c+1); b; a] *)
        pf_lst := (List.hd !pf_lst + 1) :: List.tl !pf_lst;
        create_tpl !pf_lst
      ) else (
        if List.length !pf_lst = 2 then (
          if List.nth !pf_lst 1 < sqr_ulimit then (
            (* [b; a] -> [(a+1); (a+1)] *)
            pf_lst := [(List.nth !pf_lst 1) + 1; (List.nth !pf_lst 1) + 1];
            create_tpl !pf_lst
          ) else (
            None
          )
        ) else (
          (* [c; b; a] -> [(b+1); a] *)
          pf_lst := ((List.nth !pf_lst 1) + 1) :: List.tl (List.tl !pf_lst);
          create_tpl !pf_lst
        )
      )
    )
  in
  next_pf

module IntSet = Set.Make(Int)

let solve num =
  let pf = pf_generator (num * 2) in
  let n_array = Array.make (num + 1) 0 in
  let rec loop () =
    match pf() with
    | None ->
       Array.to_list n_array |> IntSet.of_list |> IntSet.to_seq |> Seq.fold_left (+) 0
    | Some (k, v) ->
       if k <= num && (n_array.(k) = 0 || v < n_array.(k)) then (
         n_array.(k) <- v;
         loop()
       ) else
         loop()
  in
  loop()

let () =
  Printf.printf "Answer: %d\n" (solve 12_000)
