
# project euler: problem 27

#=
  n^2 + an + b,  where abs(a) < 1000 and abs(b) < 1000

  when 'n' = 0:
    '0 + 0 + b' = 'b' must be a prime number. so, 2 < 'b' < 1000.
    ('b' must not be 2 since value of the expression becomes even when 'n' is an even number)
  when 'n' = 1:
    '1 + a + b' must be a prime number. write this prime number is 'x', then 'a' = 'x' - 'b' - 1.
    abs('x' - b - 1) < 1000 and 2 < 'b' < 1000 ===> 0 < 'x' < 2000
  when 'n' is a odd number:
    'n^2 + b' is a even number. so 'a' must be a odd number.
=#

module Prob0027

import Primes: isprime, primes

function count_consec_times(a, b)
    n = 0
    while isprime(n * n + a * n + b) == true
        n += 1
    end
    n
end

function solve_0027()
    p_lst = primes(2_000)
    max_len = 0
    max_tpl = (0, 0)

    for b in filter(x -> x < 1_000, p_lst[2:end])
        for a in map(x -> x - b - 1, filter(x -> abs(x - b - 1) < 1_000, p_lst))
            len = count_consec_times(a, b)
            if len > max_len
                max_len = len
                max_tpl = (a, b)
            end
        end
    end
    max_tpl[1] * max_tpl[2]
end

end #module

using .Prob0027: solve_0027
export solve_0027
