
open Core

let run (fn: unit -> string) num =
  let t1 = Time_float.now () in
  let result = fn () in
  let elapsed_time = Time_float.diff (Time_float.now ()) t1 in
  printf "[Problem %d]\n" num;
  printf "Answer: %s\n" result;
  printf "Elapsed time: %s\n" (Time_float.Span.to_string_hum elapsed_time)
;;

let read_file filename = In_channel.with_file filename ~f:In_channel.input_lines

let read_data filename =
  let fname = ref filename in
  let speclist = [("-f", Arg.Set_string fname, "<filename>  Set input file name (If not specified, read from stdin)")] in
  Arg.parse speclist (fun _ -> ()) "Usage:";
  read_file !fname
;;
