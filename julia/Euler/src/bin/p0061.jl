
# project euler: problem 61

module Prob0061

import Combinatorics: permutations

function make_polynum_tbl()
    fn_tbl = Dict{Int, Function}(
        3 => (n) -> n * (n - 1) ÷ 2,
        4 => (n) -> n ^ 2,
        5 => (n) -> n * (3n - 1) ÷ 2,
        6 => (n) -> n * (2n - 1),
        7 => (n) -> n * (5n - 3) ÷ 2,
        8 => (n) -> n * (3n - 2)
    )

    tbl = Dict{Int, Dict{Int, Vector{Int}}}()
    for i = 3:8
        x = Dict{Int, Vector{Int}}()
        j = 0
        fn = fn_tbl[i]
        while true
            j += 1
            n = fn(j)
            if n < 1_000 || n % 100 < 10
                continue
            elseif n > 10_000
                break
            end
            k, v = divrem(n, 100)
            x[k] = push!(get(x, k, []), v)
        end
        tbl[i] = x
    end
    tbl
end

function find_cycle(polynum_tbl::Dict{Int, Dict{Int, Vector{Int}}}, route::Vector{Int})::Union{Vector{Int}, Nothing}
    function is_distinct_numbers(nums::Vector{Int})
        tmp = Set{Int}()
        for i = 1:(length(nums) - 1)
            push!(tmp, nums[i] * 100 + nums[i + 1])
        end
        length(tmp) == (length(nums) - 1)
    end

    function dfs(route::Vector{Int}, path::Vector{Int})::Union{Vector{Int}, Nothing}
        if length(route) == 0
            if path[1] == path[end] && is_distinct_numbers(path) == true
                return path[2:end]
            else
                return nothing
            end
        end

        next_map = polynum_tbl[route[1]]
        if haskey(next_map, path[end]) == false
            return nothing
        end
        for next_num in next_map[path[end]]
            result = dfs(route[2:end], vcat(path, [next_num]))
            if result !== nothing
                return result
            end
        end
        return nothing
    end

    for (k, v) in polynum_tbl[8]
        for next_num in v
            result = dfs(route, [k, next_num])
            if result !== nothing
                return result
            end
        end
    end
    return nothing
end

function solve_0061()
    polynum_tbl = make_polynum_tbl()

    for route in permutations(3:7)
        result = find_cycle(polynum_tbl, route)
        if result !== nothing
            # sum(100*x{1} + x{2}, 100*x{2} + x{3}, ..., 100*x{n} + x{1}) = sum(x{1}, x{2}, ..., x{n}) * 101
            return sum(result) * 101
        end
    end
    @assert false "not reached"
end

end #module

using .Prob0061: solve_0061
export solve_0061
