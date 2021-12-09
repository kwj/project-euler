(* Project Euler: Problem 49 *)

(*
  Is the number 3300 meaningful?
 *)

(* ---------------------------------------------------------------- *)

let era_sieve n =
  let rec sieve lst =
    match lst with
    | [] -> []
    | hd :: tl -> hd :: (sieve (List.filter (fun x -> x mod hd <> 0) tl))
  in
  sieve (List.init (n - 1) (fun x -> x + 2))

let mk_strkey number =
  let rec numlst n lst =
    if n = 0 then lst else numlst (n / 10) ((n mod 10) :: lst)
  in
  List.sort compare (numlst number [])
  |> List.fold_left (fun s n -> s ^ (string_of_int n)) ""

module IntSet = Set.Make(Int)
let find_cands p_lst =
  let size = List.length p_lst in
  let p_array = Array.of_list p_lst in
  let p_set = IntSet.of_list p_lst in
  let result = ref [] in
  for i = 0 to size - 1 do
    for j = i + 1 to size - 1 do
      let tmp = 2 * p_array.(j) - p_array.(i) in
      if IntSet.mem tmp p_set then
        if mk_strkey(p_array.(i)) = mk_strkey(p_array.(j)) && mk_strkey(p_array.(i)) = mk_strkey(tmp) then
          result := (p_array.(i), p_array.(j), 2 * p_array.(j) - p_array.(i)) :: !result
    done
  done;
  !result

let () =
  let cands = List.filter ((<) 999) (era_sieve 9999) |> find_cands in
  let rec aux lst =
    match lst with
    | (i, j, k) :: tl ->
       Printf.printf "%d%d%d (%d, %d, %d)\n" i j k i j k; aux tl
    | []
      -> ()
  in
  aux cands
