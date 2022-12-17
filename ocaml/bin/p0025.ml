(* Project Euler: Problem 25 *)

open Core

let calc_by_zarith () =
  let ulimit = Z.pow (Z.of_int 10) 999 in    (* the minimum number which contains 1000 digits is 10**999. *)
  let rec loop f1 f2 idx =
    if Z.geq f2 ulimit then
      idx
    else
      loop f2 Z.(f2 + f1) (succ idx)
  in
  loop Z.one Z.one 2
  
let calc_by_array () =
  let fib = Array.make_matrix ~dimx:3 ~dimy:(1000 + 1) 0 in    (* '+ 1' is a spare area that will never be accessed. *)
  let i = ref 0 in
  let cnt = ref 2 in
  let carry = ref 0 in
  fib.(0).(0) <- 1; fib.(1).(0) <- 1; fib.(2).(0) <- 1;
  while fib.(!i).(999) = 0 do
    i := (!i + 1) mod 3;
    cnt := !cnt + 1;
    for j = 0 to 999 do
      fib.(!i).(j) <- fib.((!i + 1) mod 3).(j) + fib.((!i + 2) mod 3).(j) + !carry;
      carry := fib.(!i).(j) / 10;
      fib.(!i).(j) <- fib.(!i).(j) mod 10
    done
  done;
  !cnt

let exec () =
  sprintf "%d (w/ Zarith module)\n%d (w/o Zarith module)" (calc_by_zarith ()) (calc_by_array ())

let () = Euler.Task.run exec