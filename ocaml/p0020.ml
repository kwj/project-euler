(* Project Euler: Problem 20 *)

let calc_by_zarith num =
  let open Z in
  let rec sum target result =
    let tmp = target / (of_int 10) in
    if tmp > Z.zero then
      sum tmp (result + (target mod (of_int 10)))
    else
      to_int (result + target)
  in
  sum (fac num) Z.zero

let calc_by_array num =
  (* According to Stirling's approximation, the number of digits in 100! is about 158.
     So, I use 160 as the size of array. *)
  let ndigits = 160 in
  let work = Array.make ndigits 0 in
  let rec factorial_of n =
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
  Array.fold_left (+) 0 (factorial_of num)

let () =
  Printf.printf "the sum of the digits in 100! is:\n";
  Printf.printf "   %d: (w/ Zarith module)\n" (calc_by_zarith 100);
  Printf.printf "   %d: (w/o Zarith module)\n" (calc_by_array 100);
