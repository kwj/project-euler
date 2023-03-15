
# project euler: problem 82

module Prob0082

function solve_0082(fname::String = "p082_matrix.txt", fn::Function = min)
    data = vcat(([parse(Int, x) for x in split(l, ",")]' for l in readlines(joinpath((@__DIR__), "../../assets", fname)))...)

    work = data[:, 1]
    for j = axes(data, 2)[begin + 1:end]
        crnt = data[:, j]
        work[begin] += crnt[begin]
        for i = axes(data, 1)[begin + 1:end]
            work[i] = crnt[i] + fn(work[i], work[i - 1])
        end
        for i = axes(data, 1)[end - 1:-1:begin]
            work[i] = fn(work[i], work[i + 1] + crnt[i])
        end
    end
    fn(work...)
end

end #module

using .Prob0082: solve_0082
export solve_0082
