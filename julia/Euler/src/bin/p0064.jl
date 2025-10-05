
# project euler: problem 64

#=
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
     a{n} = floor( (sqrt(N)+b{n}) / c{n} )
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}

     b{0} = 0, c{0} = 1, a{0} = sqrt(N)
=#

module Prob0064

function get_cont_fraction(n)
    rep = Vector{Int}(undef, 0)
    isqrt_n = isqrt(n)
    if n == isqrt_n ^ 2
        return (isqrt_n, rep)
    end

    stop_condition = 2 * isqrt_n
    b = 0
    c = 1
    a = (isqrt_n + b) ÷ c
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

function solve_0064(limit::Int = 10_000)
    cnt = 0
    for n = 1:limit
        if length(get_cont_fraction(n)[2]) % 2 == 1
            cnt += 1
        end
    end
    cnt
end

end #module

using .Prob0064: solve_0064
export solve_0064
