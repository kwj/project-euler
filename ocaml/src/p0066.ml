(* Project Euler: Problem 66 *)

(*
  X^2 - N * Y^2 = 1
  -----------------

  This equation is called Pell's equation, but I didn't know that.
  So I wrote a solution by referring Wikipedia.

    https://en.wikipedia.org/wiki/Pell%27s_equation


            sqrt(N) + b0        1              1
  sqrt(N) = ------------ = a0 + --,  x1 = a1 + --, ...
                 c0             x1             x2

                  c0             c0(sqrt(N) - (b0 - a0c0))
    x1 = --------------------- = -------------------------
         sqrt(N) + (b0 - a0c0)       N - (b0 - a0c0)^2

         sqrt(N) + (a0c0 - b0)   sqrt(N) + b1         1
       = --------------------- = ------------- = a1 + --
           N - (a0c0 - b0)^2          c1              x2
           -----------------
                  c0
   -->
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}
     a{n+1} = floor((sqrt(N)+b{n+1}) / c{n+1}) = (floor(sqrt(N)) + b{n+1}) / c{n+1}

       a{0} = floor(sqrt(N)), b{0} = 0, c{0} = 1


  write A{0}, A{1}, A{2}, ...
           x{0}                          x{1}                      1         x{2}
    a{0} = ---- = A{0},  a{0} + 1/a{1} = ---- = A{1},  a{0} + ------------ = ---- = A{2},  ...
           y{0}                          y{1}                         1      y{2}
                                                              a{1} + ----
                                                                     a{2}
                                              [x{0} = a{0}, y{0} = 1]
   -->
        n=0: -> x{0} = a{0}, y{0} = 1
        n=1: -> x{1} = a{0}*a{1} + 1, y{1} = a{1}

        n=2: -> x{2}/y{2}
                     = (a{0}*a{1}*a{2} + a{0} + a{2}) / (a{1}a{2} + 1)

                       a{2}*(a{0}*a{1} + 1) + a{0}   a{2}*x{1} + a{0}
                     = --------------------------- = ----------------
                            a{2}*a{1} + 1            a{2}*y(1) + 1

                       a{2}*x{1} + x{0}
                     = ----------------
                       a{2)*y{1} + y{0}

                                    a{k}*x{k-1} + x{k-2}
     assume that A{k} = x{k}/y{k} = --------------------  [k>=2]
                                    a{k}*y{k-1} + y{k-2}

                                 ((a{k}*a{k+1} + 1)/a{k+1})*x{k-1} + x{k-2}
        A{k+1} = x{k+1}/y{k+1} = -----------------------------------------
                                 ((a{k}*a{k+1} + 1)/a{k+1})*y{k-1} + y{k-2}

                                 (a{k}*a{k+1} + 1)*x{k-1} + x{k-2}*a{k+1}
                               = -----------------------------------------
                                 (a{k}*a{k+1} + 1)*y{k-1} + y{k-2}*a{k+1}

                                 a{k+1}(a{k}*x{k-1} + x{k-2}) + x{k-1}
                               = -------------------------------------
                                 a{k+1}(a{k}*y{k-1} + y{k-2}) + y{k-1}

                                 a{k+1}*x{k} + x{k-1}
                               = --------------------
                                 a{k+1}*y{k} + y{k-1}
      -->
        x{k+1} = a{k+1} * x{k} + x{k-1}
        y{k+1} = a{k+1} * y{k} + y{k-1}

   -->
     [a{0}; a{1], a{2}, ...]
     assume that x{-1} = 1, x{0} = a{0}, y{-1} = 0, y{0} = 1

     [n>=1]
       x{n} = a{n} * x{n-1} + x{n-2}
       y{n} = a{n} * y{n-1} + y{n-2}
*)

open Core

let get_cont_fraction n =
  let isqrt_n = Euler.Math.isqrt n in
  if Int.pow isqrt_n 2 = n
  then (isqrt_n, [])
  else (
    let stop_condition = isqrt_n * 2 in
    let rec loop a b c lst =
      let b = a * c - b in
      let c = (n - b * b) / c in
      let a = (isqrt_n + b) / c in
      if a = stop_condition
      then (isqrt_n, List.rev (a :: lst))
      else loop a b c (a :: lst)
    in
    loop isqrt_n 0 1 [])
;;

let get_numerator a0 lst =
  List.fold ~init:(Z.of_int a0, Z.one) ~f:(fun (n1, n2) n -> Z.(n1 * ~$n + n2, n1)) lst
  |> fst
;;

let compute limit =
  List.range 1 limit ~stop:`inclusive
  |> List.filter ~f:(fun d -> Int.pow (Euler.Math.isqrt d) 2 <> d)
  |> List.map ~f:(fun d ->
    let (a0, lst) = get_cont_fraction d in
    match List.length lst mod 2 with
    | 0 -> (d, get_numerator a0 (List.drop_last_exn lst))
    | _ -> (d, get_numerator a0 (List.drop_last_exn (lst @ lst))))
  |> List.max_elt ~compare:(fun (_, z1) (_, z2) -> Z.compare z1 z2)
  |> Option.value_exn
  |> fst
;;

let solve () = compute 1_000 |> Int.to_string

(* Test *)

let%test_unit "7" = [%test_eq: int] (compute 7) 5
let%test_unit "1_000" = [%test_eq: int] (compute 1_000) 661
