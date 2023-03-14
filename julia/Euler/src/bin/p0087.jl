
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
    sq_plst = primes(isqrt(thr))
    cb_plst = filter((x) -> x^3 <= thr, sq_plst)
    tsst_plst = filter((x) -> x^4 <= thr, sq_plst)

    result = Set{Int}()
    for z in tsst_plst
        for y in cb_plst
            if (tmp = z^4 + y^3) >= thr
                break
            end
            for x in sq_plst
                if tmp + x^2 < thr
                    push!(result, tmp + x^2)
                end
            end
        end
    end
    length(result)
end

end #module

using .Prob0087: solve_0087
export solve_0087
