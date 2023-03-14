
# project euler: problem 66

#=
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
=#

module Prob0066

function get_cont_fraction(n::Int)::Tuple{Int, Vector{Int}}
    isqrt_n = isqrt(n)
    if n == isqrt_n ^ 2
        return (isqrt_n, [])
    end

    stop_condition = 2 * isqrt_n
    b = 0
    c = 1
    a = (isqrt_n + b) ÷ c
    rep = Vector{Int}(undef, 0)
    while true
        b = a * c - b
        c = (n - b * b) ÷ c
        a = (isqrt_n + b) ÷ c
        push!(rep, a)
        if a == stop_condition
            return (isqrt_n, rep)
        end
    end
end

function get_numerator(a0::Int, rep_lst::Vector{Int})
    xₙ₁, xₙ₂ = big(a0), big(1)
    for a in rep_lst
        xₙ₁, xₙ₂ = a * xₙ₁ + xₙ₂, xₙ₁
    end
    xₙ₁
end

function solve_0066(limit::Int = 1_000)
    answer = (0, 0)
    for i = 1:limit
        cf = get_cont_fraction(i)
        if length(cf[2]) == 0
            continue
        elseif length(cf[2]) % 2 == 0
            numerator = get_numerator(cf[1], cf[2][1:end - 1])
        else
            numerator = get_numerator(cf[1], vcat(cf[2], cf[2])[1:end - 1])
        end
        if answer[1] < numerator
            answer = (numerator, i)
        end
    end
    answer[2]
end

end #module

using .Prob0066: solve_0066
export solve_0066
