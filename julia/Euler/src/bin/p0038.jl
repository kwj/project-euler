
# project euler: problem 38

#=
  It is clear that number X is within 4 digits from the problem statement.

    1) if number X is four digits, n = 2  (X * 1 -> 4-digits, X * 2 -> 5-digits)
    2) if number X is three digits, n = 3  (X * 1 -> 3-digits, X * 2 -> 3-digits, X * 3 -> 3-digits)
    3) if number X is two digits, n = 4  (X * 1 -> 2-digits, X * 2 -> 2-digits, X * 3 -> 2-digits, X * 4 -> 3-digits)
    4) if number X is one digit, n = 9 or 5 (only X=1 and n=9, X=9 and n=5).

  case #1:
    5000 <= X <= 9999
  case #2:
    100 <= X <= 333
  case #3:
    25 <= X <= 33
  case #4:
    X = 1, 9
=#

module Prob0038

import ..Util: is_pandigital_nz

function solve_0038()
    answer = []
    for (m, (start, stop)) in [(2, (5_000, 9_999)), (3, (100, 333)), (4, (25, 33)), (5, (9, 9)), (9, (1, 1))]
        for x = start:stop
            n = parse(Int, reduce(*, string(x * k) for k = 1:m))
            if is_pandigital_nz(n) == true
                push!(answer, n)
            end
        end
    end

    maximum(answer)
end

end #module

using .Prob0038: solve_0038
export solve_0038
