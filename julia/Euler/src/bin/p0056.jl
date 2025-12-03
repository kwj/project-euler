
# project euler: problem 56

module Prob0056

function solve_0056()
    answer = 0
    for a = reverse(1:BigInt(99))
        # assume that x = 10 * n
        # x^y = (10 * n)^y = 10^y * n^y, so sum(digits(x^y)) = sum(digits(n^y))
        # we can skip to check multiples of ten in this problem.
        if a % 10 == 0
            continue
        end
        for b = reverse(1:BigInt(99))
            p = a ^ b
            if length(string(p)) * 9 < answer
                break
            end
            answer = max(sum(digits(p)), answer)
        end
    end
    answer
end

end #module

using .Prob0056: solve_0056
export solve_0056
