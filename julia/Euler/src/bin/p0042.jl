
# project euler: problem 42

module Prob0042

import ..Util: is_triangular

function solve_0042(fname::String = "p042_words.txt")
    worth(word) = sum(Int(x) - Int('A') + 1 for x in word)
    data = readline(joinpath((@__DIR__), "../../assets", fname))

    count(is_triangular, worth.(split(replace(data, "\"" => ""), ",")))
end

end #module

using .Prob0042: solve_0042
export solve_0042
