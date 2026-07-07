type t =
  { word_size : int
  ; n_elems : int
  ; arr : int array
  }

let init n =
  let word_size = Sys.int_size in
  let arr_size = (n + word_size) / word_size in
  { word_size; n_elems = n; arr = Array.make arr_size 0 }
;;

let set ba n =
  assert (n >= 0 && n <= ba.n_elems);
  let word_idx = n / ba.word_size
  and bit_idx = n mod ba.word_size in
  ba.arr.(word_idx) <- ba.arr.(word_idx) lor Int.shift_left 1 bit_idx
;;

let clear ba n =
  assert (n >= 0 && n <= ba.n_elems);
  let word_idx = n / ba.word_size
  and bit_idx = n mod ba.word_size in
  ba.arr.(word_idx) <- (ba.arr.(word_idx) land Int.(lognot (shift_left 1 bit_idx)))
;;

let test ba n =
  assert (n >= 0 && n <= ba.n_elems);
  let word_idx = n / ba.word_size
  and bit_idx = n mod ba.word_size in
  ba.arr.(word_idx) land Int.shift_left 1 bit_idx <> 0
;;

let popcount ba = Array.fold_left (fun acc x -> acc + Int.popcount x) 0 ba.arr
