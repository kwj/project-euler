
# project euler: problem 67

module Prob0067

function select_leaf(fn, lst)
    result = eltype(lst)[]
    prev = lst[1]
    for i in lst
        push!(result, fn(prev, i))
        prev = i
    end
    result[2:end]
end

function solve_0067(fname::String = "p067_triangle.txt", fn::Function = max)
    data = reverse(map.((x) -> parse(Int, x), split.(readlines(joinpath((@__DIR__), "../../assets", fname)), " ")))

    prev = zeros(Int, length(data[1]) + 1)
    for lst in data
        selected = select_leaf(fn, prev)
        prev = map(+, lst, selected)
    end
    prev[1]
end

end #module

using .Prob0067: solve_0067
export solve_0067
