
# project euler: problem 4

module Prob0004

import ..Util: is_palindrome

function solve_0004(n_digits::Int = 3)
    @assert n_digits > 0 "Range Error: The digits parameter must be larger than 0. [$n_digits]"
    num_upper = 10 ^ n_digits - 1
    num_lower = n_digits == 1 ? 0 : 10 ^ (n_digits - 1)
    blk_width = 10 ^ (n_digits * 2 - 2)

    answer = Int[]
    for blk_lower in reverse((num_lower ^ 2):blk_width:(num_upper ^ 2))
        blk_upper = blk_lower + blk_width - 1
        for x = reverse(num_lower:num_upper)
            if x * x < blk_lower
                break
            end
            for y = reverse(num_lower:(x == 0 ? x : min(blk_upper ÷ x, x)))
                tmp = x * y
                if tmp < blk_lower
                    break
                end
                if is_palindrome(tmp) == true
                    push!(answer, tmp)
                end
            end
        end
        if length(answer) > 0
            return maximum(answer)
        end
    end

    error("Not found")
end

end #module

using .Prob0004: solve_0004
export solve_0004
