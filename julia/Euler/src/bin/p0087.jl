
# project euler: problem 87

#=
  >>> 50000000 ** (1/2)
  7071.067811865475
  >>> 50000000 ** (1/3)
  368.40314986403854
  >>> 50000000 ** (1/4)
  84.08964152537145
=#

module Prob0087

import Primes: primes

function solve_0087(thr::Int = 50_000_000)
    p_lst = primes(isqrt(thr))
    sq_plst = filter(x -> x < thr, map(x -> x^2, p_lst))
    cb_plst = filter(x -> x < thr, map(x -> x^3, p_lst))
    qu_plst = filter(x -> x < thr, map(x -> x^4, p_lst))

    result = Set{Int}()
    for z⁴ in qu_plst
        for y³ in cb_plst
            (tmp = z⁴ + y³) >= thr && break
            for x² in sq_plst
                tmp + x² >= thr && break

                push!(result, tmp + x²)
            end
        end
    end
    length(result)
end

end #module

using .Prob0087: solve_0087
export solve_0087
