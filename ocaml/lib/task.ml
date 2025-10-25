open Core

let run (fn : unit -> string) =
  let t1 = Time_float.now () in
  let result = fn () in
  let elapsed_time =
    Time_float.diff (Time_float.now ()) t1 |> Time_float.Span.to_string_hum
  in
  (result, elapsed_time)
;;

let run_with_file (fn : string -> string) fname =
  let t1 = Time_float.now () in
  let result = fn fname in
  let elapsed_time =
    Time_float.diff (Time_float.now ()) t1 |> Time_float.Span.to_string_hum
  in
  (result, elapsed_time)
;;

let read_file filename = In_channel.with_file filename ~f:In_channel.input_lines
