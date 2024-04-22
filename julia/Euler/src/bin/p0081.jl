
# project euler: problem 81

module Prob0081

function solve_0081(fname::String = "0081_matrix.txt", fn::Function = min)
    data = vcat(([parse(Int, x) for x in split(l, ",")]' for l in readlines(joinpath((@__DIR__), "../../assets", fname)))...)

    data[1, :] = accumulate(+, data[1, :])
    data[:, 1] = accumulate(+, data[:, 1])
    for i = axes(data, 1)[begin + 1:end]
        for j = axes(data, 2)[begin + 1:end]
            data[i, j] += fn(data[i - 1, j], data[i, j - 1])
        end
    end
    data[end, end]
end

end #module

using .Prob0081: solve_0081
export solve_0081
