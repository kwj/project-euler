
# project euler: problem 26

module Prob0026

import Primes: factor
import ..Util: divisors

# preprocessing
function pp(n)
    while n % 2 == 0
        n ÷= 2
    end
    while n % 5 == 0
        n ÷= 5
    end
    n
end

function find_repetend_length(n)
    # This function is not strictly the correct Carmichael function
    # because the function assumes that the argument is not a multiple of 2.
    carmichael(n) = lcm(map(((b, e),) -> (b - 1) * (b ^ (e - 1)), collect(factor(n))))

    n = pp(n)
    if n == 1
        return 0
    end
    for k in divisors(carmichael(n))
        if powermod(10, k, n) == 1
            return k
        end
    end

    error("Not Reached")
end

function solve_0026(upper::Int = 1_000)
    (max_length, answer) = (0, 0)

    for d = reverse(div(upper, 2):upper - 1)
        if d <= max_length
            break
        end
        repetend_length = find_repetend_length(d)
        if repetend_length > max_length
            (max_length, answer) = (repetend_length, pp(d))
        end
    end
    answer
end

end #module

using .Prob0026: solve_0026
export solve_0026
