
# project euler: problem 4

module Prob0004

import ..Util: is_palindrome

function solve_0004(n_digits = 3)
    @assert n_digits > 0 "Range Error: The digits parameter must be larger than 0. [$n_digits]"
    num_upper = 10 ^ n_digits - 1
    num_lower = n_digits == 1 ? 1 : 10 ^ (n_digits - 1)
    blk_upper_limit = 10 ^ (n_digits * 2)
    blk_lower_limit = n_digits > 1 ? 10 ^ ((n_digits - 1) * 2) : 0
    blk_width = 10 ^ (n_digits * 2 - 2)
    answer = Int[]

    for blk_upper in StepRange(blk_upper_limit, -blk_width, blk_lower_limit)
        blk_lower = blk_upper - blk_width
        for x in StepRange(num_upper, -1, num_lower)
            if x * x < blk_lower
                break
            end
            for y in StepRange(min(blk_upper ÷ x, x), -1, num_lower)
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

    @assert false "Not find"
end

end #module

using .Prob0004: solve_0004
export solve_0004
