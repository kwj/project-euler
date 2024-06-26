
# project euler: problem 22

module Prob0022

function solve_0022(fname::String = "0022_names.txt")
    worth(word) = sum(Int(x) - Int('A') + 1 for x in word)
    data = readline(joinpath((@__DIR__), "../../assets", fname))

    sum(idx * v for (idx, v) in pairs(worth.(sort(split(replace(data, "\"" => ""), ",")))))
end

end #module

using .Prob0022: solve_0022
export solve_0022
