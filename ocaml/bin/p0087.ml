(* Project Euler: Problem 87 *)

(*
  >>> 50000000 ** (1/2)
  7071.067811865475
  >>> 50000000 ** (1/3)
  368.40314986403854
  >>> 50000000 ** (1/4)
  84.08964152537145

  This program is a bit slow. Here is a result on Raspberry Pi 4 Model B.

    % time ./_build/default/bin/p0087.exe
    1097343
    Elapsed time: 2.697148323059082s
    ./_build/default/bin/p0087.exe  2.52s user 0.21s system 99% cpu 2.741 total
 *)

open Core

let solve limit =
  let pp_triples x y z =
    (x * x) + (y * y * y) + (z * z * z * z)
  in
  let x_primes = Euler.Eratosthenes.to_list (Euler.Eratosthenes.generate (Euler.Math.isqrt limit)) in
  let y_primes = List.take_while ~f:(fun n -> n <= Float.(iround_exn ~dir:`Down (of_int limit ** (1. / 3.)))) x_primes in
  let z_primes = List.take_while ~f:(fun n -> n <= Float.(iround_exn ~dir:`Down (of_int limit ** (1. / 4.)))) x_primes in

  let rec loop_z z_lst acc =
    let rec loop_y z y_lst acc =
      let rec loop_x z y x_lst acc =
        match x_lst with
          [] -> acc
        | x :: xs -> let tmp = pp_triples x y z in
                     if tmp >= limit then
                       acc
                     else
                       loop_x z y xs (tmp :: acc)
      in
      match y_lst with
        [] -> acc
      | y :: ys -> loop_y z ys (loop_x z y x_primes acc)
    in
    match z_lst with
      [] -> acc
    | z :: zs -> loop_z zs (loop_y z y_primes acc)
  in
  List.dedup_and_sort (loop_z z_primes []) ~compare |> List.length

let exec () =
  Int.to_string (solve (50_000_000))

let () = Euler.Task.run exec
