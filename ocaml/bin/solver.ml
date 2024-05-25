(* Project Euler: main program *)

open Core

let usage_msg = "solver <problem number> [-f data_file]"
let problem_number = ref []
let data_file = ref ""
let anon_fn p_number = problem_number := p_number :: !problem_number
let specs = [ ("-f", Arg.Set_string data_file, "Set data file name") ]

let print_result num result elapsed_time =
  printf "[Problem %d]\n" num;
  printf "Answer: %s\n" result;
  printf "Elapsed time: %s\n" elapsed_time
;;

let () =
  Arg.parse specs anon_fn usage_msg;
  if List.length !problem_number <> 1
  then exit 1
  else (
    match int_of_string_opt (List.hd_exn !problem_number) with
    | Some n ->
      let result, elapsed_time =
        match n with
        | 1 -> Euler.Task.run Solution.P0001.solve
        | 2 -> Euler.Task.run Solution.P0002.solve
        | 3 -> Euler.Task.run Solution.P0003.solve
        | 4 -> Euler.Task.run Solution.P0004.solve
        | 5 -> Euler.Task.run Solution.P0005.solve
        | 6 -> Euler.Task.run Solution.P0006.solve
        | 7 -> Euler.Task.run Solution.P0007.solve
        | 8 -> Euler.Task.run Solution.P0008.solve
        | 9 -> Euler.Task.run Solution.P0009.solve
        | 10 -> Euler.Task.run Solution.P0010.solve
        | 11 -> Euler.Task.run Solution.P0011.solve
        | 12 -> Euler.Task.run Solution.P0012.solve
        | 13 -> Euler.Task.run Solution.P0013.solve
        | 14 -> Euler.Task.run Solution.P0014.solve
        | 15 -> Euler.Task.run Solution.P0015.solve
        | 16 -> Euler.Task.run Solution.P0016.solve
        | 17 -> Euler.Task.run Solution.P0017.solve
        | 18 -> Euler.Task.run Solution.P0018.solve
        | 19 -> Euler.Task.run Solution.P0019.solve
        | 20 -> Euler.Task.run Solution.P0020.solve
        | 21 -> Euler.Task.run Solution.P0021.solve
        | 22 ->
          if String.compare !data_file "" = 0
          then Euler.Task.run_with_file Solution.P0022.solve "./src/assets/p022_names.txt"
          else Euler.Task.run_with_file Solution.P0022.solve !data_file
        | 23 -> Euler.Task.run Solution.P0023.solve
        | 24 -> Euler.Task.run Solution.P0024.solve
        | 25 -> Euler.Task.run Solution.P0025.solve
        | 26 -> Euler.Task.run Solution.P0026.solve
        | 27 -> Euler.Task.run Solution.P0027.solve
        | 28 -> Euler.Task.run Solution.P0028.solve
        | 29 -> Euler.Task.run Solution.P0029.solve
        | 30 -> Euler.Task.run Solution.P0030.solve
        | 31 -> Euler.Task.run Solution.P0031.solve
        | 32 -> Euler.Task.run Solution.P0032.solve
        | 33 -> Euler.Task.run Solution.P0033.solve
        | 34 -> Euler.Task.run Solution.P0034.solve
        | 35 -> Euler.Task.run Solution.P0035.solve
        | 36 -> Euler.Task.run Solution.P0036.solve
        | 37 -> Euler.Task.run Solution.P0037.solve
        | 38 -> Euler.Task.run Solution.P0038.solve
        | 39 -> Euler.Task.run Solution.P0039.solve
        | 40 -> Euler.Task.run Solution.P0040.solve
        | 41 -> Euler.Task.run Solution.P0041.solve
        | 42 ->
          if String.compare !data_file "" = 0
          then Euler.Task.run_with_file Solution.P0042.solve "./src/assets/p042_words.txt"
          else Euler.Task.run_with_file Solution.P0042.solve !data_file
        | 43 -> Euler.Task.run Solution.P0043.solve
        | 44 -> Euler.Task.run Solution.P0044.solve
        | 45 -> Euler.Task.run Solution.P0045.solve
        | 46 -> Euler.Task.run Solution.P0046.solve
        | 47 -> Euler.Task.run Solution.P0047.solve
        | 48 -> Euler.Task.run Solution.P0048.solve
        | 49 -> Euler.Task.run Solution.P0049.solve
        | 50 -> Euler.Task.run Solution.P0050.solve
        | 51 -> Euler.Task.run Solution.P0051.solve
        | 52 -> Euler.Task.run Solution.P0052.solve
        | 53 -> Euler.Task.run Solution.P0053.solve
        | 54 ->
          if String.compare !data_file "" = 0
          then Euler.Task.run_with_file Solution.P0054.solve "./src/assets/p054_poker.txt"
          else Euler.Task.run_with_file Solution.P0054.solve !data_file
        | 55 -> Euler.Task.run Solution.P0055.solve
        | 56 -> Euler.Task.run Solution.P0056.solve
        | 57 -> Euler.Task.run Solution.P0057.solve
        | 58 -> Euler.Task.run Solution.P0058.solve
        | 59 ->
          if String.compare !data_file "" = 0
          then
            Euler.Task.run_with_file Solution.P0059.solve "./src/assets/p059_cipher.txt"
          else Euler.Task.run_with_file Solution.P0059.solve !data_file
        | 60 -> Euler.Task.run Solution.P0060.solve
        | 61 -> Euler.Task.run Solution.P0061.solve
        | 62 -> Euler.Task.run Solution.P0062.solve
        | 63 -> Euler.Task.run Solution.P0063.solve
        | 64 -> Euler.Task.run Solution.P0064.solve
        | 65 -> Euler.Task.run Solution.P0065.solve
        | 66 -> Euler.Task.run Solution.P0066.solve
        | 67 ->
          if String.compare !data_file "" = 0
          then
            Euler.Task.run_with_file Solution.P0067.solve "./src/assets/p067_triangle.txt"
          else Euler.Task.run_with_file Solution.P0067.solve !data_file
        | 68 -> Euler.Task.run Solution.P0068.solve
        | 69 -> Euler.Task.run Solution.P0069.solve
        | 70 -> Euler.Task.run Solution.P0070.solve
        | 71 -> Euler.Task.run Solution.P0071.solve
        | 72 -> Euler.Task.run Solution.P0072.solve
        | 73 -> Euler.Task.run Solution.P0073.solve
        | 74 -> Euler.Task.run Solution.P0074.solve
        | 75 -> Euler.Task.run Solution.P0075.solve
        | 76 -> Euler.Task.run Solution.P0076.solve
        | 77 -> Euler.Task.run Solution.P0077.solve
        | 78 -> Euler.Task.run Solution.P0078.solve
        | 79 ->
          if String.compare !data_file "" = 0
          then
            Euler.Task.run_with_file Solution.P0079.solve "./src/assets/p079_keylog.txt"
          else Euler.Task.run_with_file Solution.P0079.solve !data_file
        | 80 -> Euler.Task.run Solution.P0080.solve
        | 81 ->
          if String.compare !data_file "" = 0
          then
            Euler.Task.run_with_file Solution.P0081.solve "./src/assets/p081_matrix.txt"
          else Euler.Task.run_with_file Solution.P0081.solve !data_file
        | 82 ->
          if String.compare !data_file "" = 0
          then
            Euler.Task.run_with_file Solution.P0082.solve "./src/assets/p082_matrix.txt"
          else Euler.Task.run_with_file Solution.P0082.solve !data_file
        | 83 ->
          if String.compare !data_file "" = 0
          then
            Euler.Task.run_with_file Solution.P0083.solve "./src/assets/p083_matrix.txt"
          else Euler.Task.run_with_file Solution.P0083.solve !data_file
        | 84 -> Euler.Task.run Solution.P0084.solve
        | 85 -> Euler.Task.run Solution.P0085.solve
        | 86 -> Euler.Task.run Solution.P0086.solve
        | 87 -> Euler.Task.run Solution.P0087.solve
        | 88 -> Euler.Task.run Solution.P0088.solve
        | 89 ->
          if String.compare !data_file "" = 0
          then Euler.Task.run_with_file Solution.P0089.solve "./src/assets/p089_roman.txt"
          else Euler.Task.run_with_file Solution.P0089.solve !data_file
        | 90 -> Euler.Task.run Solution.P0090.solve
        | 91 -> Euler.Task.run Solution.P0091.solve
        | 92 -> Euler.Task.run Solution.P0092.solve
        | 93 -> Euler.Task.run Solution.P0093.solve
        | 94 -> Euler.Task.run Solution.P0094.solve
        | 95 -> Euler.Task.run Solution.P0095.solve
        | 96 ->
          if String.compare !data_file "" = 0
          then
            Euler.Task.run_with_file Solution.P0096.solve "./src/assets/p096_sudoku.txt"
          else Euler.Task.run_with_file Solution.P0096.solve !data_file
        | 97 -> Euler.Task.run Solution.P0097.solve
        | 98 ->
          if String.compare !data_file "" = 0
          then Euler.Task.run_with_file Solution.P0098.solve "./src/assets/p098_words.txt"
          else Euler.Task.run_with_file Solution.P0098.solve !data_file
        | 99 ->
          if String.compare !data_file "" = 0
          then
            Euler.Task.run_with_file Solution.P0099.solve "./src/assets/p099_base_exp.txt"
          else Euler.Task.run_with_file Solution.P0099.solve !data_file
        | 100 -> Euler.Task.run Solution.P0100.solve
        | _ -> raise (Invalid_argument "No solution exists for this problem number.")
      in
      print_result n result elapsed_time
    | None -> raise (Invalid_argument "It is not a problem number."))
;;
