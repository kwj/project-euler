
# project euler: problem 90

module Prob0090

import Combinatorics: combinations, with_replacement_combinations

function solve_0090()
    function check_dice(dice, squares)
        all(squares) do sq_digits
            any(sq_digits) do sq
                sq[1] ∈ dice[1] && sq[2] ∈ dice[2]
            end
        end
    end

    # julia> squares
    # 9-element Vector{Tuple{Vector{Int64}, Vector{Int64}}}:
    # ([1, 0], [0, 1])
    # ([4, 0], [0, 4])
    # ([6, 0], [0, 6])
    # ([6, 1], [1, 6])
    # ([5, 2], [2, 5])
    # ([6, 3], [3, 6])
    # ([6, 4], [4, 6])
    # ([4, 6], [6, 4])
    # ([1, 8], [8, 1])
    squares = [(sq, reverse(sq)) for sq in replace.([digits(i^2; pad = 2) for i = 1:9], 9 => 6)]

    # julia> for dice in with_replacement_combinations(collect(combinations(vcat(0:8, 6), 6)), 2)
    #            println(dice)
    #        end
    #   ...
    # [[0, 1, 2, 3, 4, 5], [0, 1, 3, 7, 8, 6]]
    # [[0, 1, 2, 3, 4, 5], [0, 1, 4, 5, 6, 7]]
    # [[0, 1, 2, 3, 4, 5], [0, 1, 4, 5, 6, 8]]
    # [[0, 1, 2, 3, 4, 5], [0, 1, 4, 5, 6, 6]]
    # [[0, 1, 2, 3, 4, 5], [0, 1, 4, 5, 7, 8]]
    # [[0, 1, 2, 3, 4, 5], [0, 1, 4, 5, 7, 6]]
    # [[0, 1, 2, 3, 4, 5], [0, 1, 4, 5, 8, 6]]
    # [[0, 1, 2, 3, 4, 5], [0, 1, 4, 6, 7, 8]]
    # [[0, 1, 2, 3, 4, 5], [0, 1, 4, 6, 7, 6]]
    # [[0, 1, 2, 3, 4, 5], [0, 1, 4, 6, 8, 6]]
    # [[0, 1, 2, 3, 4, 5], [0, 1, 4, 7, 8, 6]]
    #   ...
    #
    # It can be seen that 6 is being used instead of 9.
    count(with_replacement_combinations(collect(combinations(vcat(0:8, 6), 6)), 2)) do dice
        check_dice(dice, squares)
    end
end

end #module

using .Prob0090: solve_0090
export solve_0090
