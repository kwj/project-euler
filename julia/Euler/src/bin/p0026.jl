
# project euler: problem 26

module Prob0026

import Primes: factor
import ..Util: divisors

@inline function pp(n)
    while n % 2 == 0
        n ÷= 2
    end
    while n % 5 == 0
        n ÷= 5
    end
    n
end

function totient(n)
    result = 1
    for (b, e) in factor(Dict, n)
        result *= (b - 1) * (b ^ (e - 1))
    end
    result
end

function find_repetend_length(d)
    d = pp(d)
    if d == 1
        return 0
    end
    for k in divisors(totient(d))
        if powermod(10, k, d) == 1
            return k
        end
    end
    @assert false "Not Reached"
end

function solve_0026(upper::Int = 1_000)
    (max_length, idx) = (0, 0)

    for i = (upper - 1):-1:(upper ÷ 2)
        if i <= max_length
            break
        end
        repetend_length = find_repetend_length(i)
        if repetend_length > max_length
            (max_length, idx) = (repetend_length, pp(i))
        end
    end
    idx
end

end #module

using .Prob0026: solve_0026
export solve_0026
