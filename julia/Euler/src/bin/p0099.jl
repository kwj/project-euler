
# project euler: problem 99

module Prob0099

function solve_0099(fname::String = "0099_base_exp.txt")
    answer = 0
    max_value = 0
    for (idx, x) in pairs([parse(Int, x) for x in split(l, ",")] for l in readlines(joinpath((@__DIR__), "../../assets", fname)))
        if (tmp = x[2] * log10(x[1])) > max_value
            answer = idx
            max_value = tmp
        end
    end
    answer
end

end #module

using .Prob0099: solve_0099
export solve_0099
