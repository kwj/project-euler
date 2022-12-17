(* Project Euler: Problem 20 *)

open Core

let calc_by_zarith num =
  let open Z in
  let rec sum target result =
    let tmp = target / (of_int 10) in
    if gt tmp Z.zero then
      sum tmp (result + (target mod (of_int 10)))
    else
      to_int (result + target)
  in
  sum (fac num) Z.zero

let calc_by_array num =
  (* According to Stirling's approximation, the number of digits in 100! is about 158.
     So, I use 160 as the size of array. *)
  let ndigits = 160 in
  let work = Array.create ~len:ndigits 0 in
  let factorial_of n =
    let carry = ref 0 in
    for factor = 1 to n do
      for i = 0 to (ndigits - 1) do
        work.(i) <- work.(i) * factor + !carry;
        carry := work.(i) / 10;
        work.(i) <- work.(i) mod 10
      done
    done;
    work
  in
  work.(0) <- 1;
  Array.fold ~f:(+) ~init:0 (factorial_of num)

let exec () =
  sprintf "%d (w/ Zarith module)\n%d (w/o Zarith module)" (calc_by_zarith 100) (calc_by_array 100)

let () = Euler.Task.run exec
