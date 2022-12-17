(* Project Euler: Problem 18 *)

open Core

let triangle = [
    [75];
    [95; 64];
    [17; 47; 82];
    [18; 35; 87; 10];
    [20; 04; 82; 47; 65];
    [19; 01; 23; 75; 03; 34];
    [88; 02; 77; 73; 07; 63; 67];
    [99; 65; 04; 28; 06; 16; 70; 92];
    [41; 41; 26; 56; 83; 40; 80; 70; 33];
    [41; 48; 72; 33; 47; 32; 37; 16; 94; 29];
    [53; 71; 44; 65; 25; 43; 91; 52; 97; 51; 14];
    [70; 11; 33; 28; 77; 73; 17; 78; 39; 68; 17; 57];
    [91; 71; 52; 38; 17; 14; 91; 43; 58; 50; 27; 29; 48];
    [63; 66; 04; 68; 89; 53; 67; 30; 73; 16; 69; 87; 40; 31];
    [04; 62; 98; 27; 23; 09; 70; 98; 73; 93; 38; 53; 60; 04; 23]
  ]

let calc_from_bottom l_lst =
  let add_max_leaf a b =
    let rec select_leaf lst =
      match lst with
      | x :: y :: [] -> [max x y]
      | x :: (y :: _ as tl) -> (max x y) :: (select_leaf tl)
      | _ -> assert false
    in
    List.map2_exn a (select_leaf b) ~f:(+)
  in
  List.fold_right l_lst
    ~f:add_max_leaf
    ~init:(List.init ((List.length @@ List.hd_exn @@ List.rev l_lst) + 1) ~f:(fun _ -> 0))
  |> List.hd_exn

let exec () =
  Int.to_string (calc_from_bottom triangle)

let () = Euler.Task.run exec
