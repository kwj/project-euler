(* Project Euler: Problem 25 *)

let calc_by_zarith () =
  let ulimit = Z.pow (Z.of_int 10) 999 in    (* the minimum number which contains 1000 digits is 10**999. *)
  let fib = Array.make 3 (Z.of_int 1) in
  let i = ref 0 in
  let cnt = ref 2 in
  while fib.(!i) < ulimit do
    i := (!i + 1) mod 3;
    cnt := !cnt + 1;
    fib.(!i) <- Z.add fib.((!i + 1) mod 3) fib.((!i + 2) mod 3)
  done;
  !cnt

let calc_by_array () =
  let fib = Array.make_matrix 3 (1000 + 1) 0 in    (* '+ 1' is a spare area that will never be accessed. *)
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

let () =
  Printf.printf "the index of the first term in the Fibonacci sequence to contain 1000 digits is:\n";
  Printf.printf "   %d: (w/ Zarith module)\n" (calc_by_zarith());
  Printf.printf "   %d: (w/o Zarith module)\n" (calc_by_array())
