(* Project Euler: Problem 16 *)

open Core

(*
  // number of digits of 2^1000
  1 + floor(log10(2**1000)) = 1 + floor(1000 * log10(2)) = 1 + floor(1000 * 0.30102...) = 1 + 301 = 302
 *)

let calc_by_zarith num =
  let open Z in
  let rec sum target result =
    let tmp = target / (of_int 10) in
    if gt tmp Z.zero then
      sum tmp (result + (target mod (of_int 10)))
    else
      to_int (result + target)
  in
  sum (pow (of_int 2) num) Z.zero

let calc_by_array num =
  let ndigits = 1 + Float.iround_towards_zero_exn ((Float.of_int num) *. (Float.log10 2.)) + 1 in    (* add one more digit as sentinel *)
  let work = Array.create ~len:ndigits 0 in
  let rec power_of_two exp =
    if exp > 0 then (
      let carry = ref 0 in
      for i = 0 to ndigits - 2 do
        work.(i) <- work.(i) * 2 + !carry;
        carry := work.(i) / 10;
        work.(i) <- work.(i) mod 10
      done;
      power_of_two (pred exp)
    ) else
      work
  in
  work.(0) <- 1;
  Array.fold ~f:(+) ~init:0 (power_of_two num)

let exec () =
  sprintf "%d (w/ Zarith module)\n%d (w/o Zarith module)" (calc_by_zarith 1_000) (calc_by_array 1_000)

let () = Euler.Task.run exec
