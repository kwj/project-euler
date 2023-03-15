
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
            for d2 in @view lst[i+1:end]
                next_lst = setdiff(lst, [d1, d2])
                aux(vcat(next_lst, [d1 + d2]))
                aux(vcat(next_lst, [d1 * d2]))
                aux(vcat(next_lst, [d1 - d2]))
                aux(vcat(next_lst, [d2 - d1]))
                if d1 != 0
                    aux(vcat(next_lst, [d2 / d1]))
                end
                if d2 != 0
                    aux(vcat(next_lst, [d1 / d2]))
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
