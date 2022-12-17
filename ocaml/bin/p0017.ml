(* Project Euler: Problem 17 *)

let words = [
    (1, String.length "one");
    (2, String.length "two");
    (3, String.length "three");
    (4, String.length "four");
    (5, String.length "five");
    (6, String.length "six");
    (7, String.length "seven");
    (8, String.length "eight");
    (9, String.length "nine");
    (10, String.length "ten");
    (11, String.length "eleven");
    (12, String.length "twelve");
    (13, String.length "thirteen");
    (14, String.length "fourteen");
    (15, String.length "fifteen");
    (16, String.length "sixteen");
    (17, String.length "seventeen");
    (18, String.length "eighteen");
    (19, String.length "nineteen");
    (20, String.length "twenty");
    (30, String.length "thirty");
    (40, String.length "forty");
    (50, String.length "fifty");
    (60, String.length "sixty");
    (70, String.length "seventy");
    (80, String.length "eighty");
    (90, String.length "ninety");
    (0, 0)    (* special data *)
  ]

let count_chars num =
  let rec aux num cnt =
    if num = 0 then
      cnt
    else
      match num with
      | 1000 ->
         (* one thousand *)
         aux (pred num) (cnt + 11)
      | n when n < 20 ->
         aux (pred num) (cnt + (List.assoc n words))
      | n when n < 100 ->
         aux (pred num) (cnt + (List.assoc (n - (n mod 10)) words) + (List.assoc (n mod 10) words))
      | n when (n mod 100) = 0 ->
         (* X00 hundred *)
         aux (pred num) (cnt + (List.assoc (n / 100) words) + 7)
      | n when (n mod 100) < 20 ->
         (* XYZ hundred and ...  (YZ < 20) *)
         aux (pred num) (cnt + (List.assoc (n / 100) words) + 7 + 3 +
                           (List.assoc (n mod 100) words))
      | n ->
         (* XYZ hundred and ...  (20 <= YZ <= 99) *)
         aux (pred num) (cnt + (List.assoc (n / 100) words) + 7 + 3 +
                           (List.assoc ((n mod 100) - (n mod 10)) words) + (List.assoc (n mod 10) words))
  in
  aux num 0

let () =
  Printf.printf "all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?\n";
  Printf.printf "   answer: %d\n" (count_chars 1000)
