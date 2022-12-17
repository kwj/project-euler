
open Core

let read_data () =
  let filename = ref "" in
  let input_files = ref [] in
  let anon_fun n = input_files := n :: !input_files in
  let speclist = [("-f", Arg.Set_string filename, "<filename>  Set input file name (If not specified, read from stdin)")] in
  Arg.parse speclist anon_fun "Usage:";
  if String.(<>) !filename "" then
    let fin = In_channel.create !filename in
    let rec loop acc =
      match In_channel.(input_line_exn fin) with
      | l -> loop (l :: acc)
      | exception End_of_file -> In_channel.close fin; List.rev acc
    in
    loop []
  else
    let rec loop acc =
      match In_channel.(input_line_exn stdin) with
      | l -> loop (l :: acc)
      | exception End_of_file -> List.rev acc
    in
    loop []

let run (fn: unit -> string) =
  let t1 = Time.now () in
  let result = fn () in
  let elapsed_time = Time.diff (Time.now ()) t1 in
  print_endline result;
  printf "Elapsed time: %s\n" (Time.Span.to_string elapsed_time)

let run_with_data (fn: string list -> string) data =
  let t1 = Time.now () in
  let result = fn data in
  let elapsed_time = Time.diff (Time.now ()) t1 in
  print_endline result;
  printf "Elapsed time: %s\n" (Time.Span.to_string elapsed_time)
