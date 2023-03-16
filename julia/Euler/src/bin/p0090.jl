
# project euler: problem 90

module Prob0090

import Combinatorics: combinations, with_replacement_combinations

function solve_0090()
    function check_square(dice::Vector{Vector{Int}})
        all(any(all(num in die for (num, die) in zip(sq, dice)) for sq in [sq_digits, reverse(sq_digits)]) for sq_digits in squares)
    end

    squares = replace.([digits(i^2; pad = 2) for i = 1:9], 9 => 6)
    count(dice -> check_square(dice), with_replacement_combinations(collect(combinations(vcat(collect(0:8), [6]), 6)), 2))
end

end #module

using .Prob0090: solve_0090
export solve_0090
