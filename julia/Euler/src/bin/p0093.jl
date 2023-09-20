
# project euler: problem 93

module Prob0093

import Combinatorics: combinations

function make_numbers(lst)
    function aux(lst)
        if length(lst) == 1
            if denominator(lst[1]) == 1
                push!(result, numerator(lst[1]))
            end
            return
        end

        for (i, d1) in pairs(lst)
            for (j, d2) in pairs(lst[i+1:end])
                next_lst = deleteat!(copy(lst), [i, i+j])
                aux(vcat([d1 + d2], next_lst))
                aux(vcat([d1 * d2], next_lst))
                aux(vcat([d1 - d2], next_lst))
                aux(vcat([d2 - d1], next_lst))
                if d1 != 0
                    aux(vcat([d2 / d1], next_lst))
                end
                if d2 != 0
                    aux(vcat([d1 / d2], next_lst))
                end
            end
        end
        return
    end

    result = Set{Int}()
    aux(lst)
    result
end

function get_consec_counts(lst)
    n_set = make_numbers([x // 1 for x in lst])
    for cnt in Iterators.countfrom(1)
        if cnt in n_set
            continue
        end
        return cnt - 1
    end
end

function solve_0093()
    foldl((acc, x) -> acc * 10 + x, maximum(map((x) -> (get_consec_counts(x), x), combinations(collect(1:9), 4)))[2])
end

end #module

using .Prob0093: solve_0093
export solve_0093
