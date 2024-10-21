
# project euler: problem 61

module Prob0061

function make_polynum_tbl(max_nsides_polygon)
    tbl = Dict{Int, Dict{Int, Vector{Int}}}()
    for i = 3:max_nsides_polygon
        x = Dict{Int, Vector{Int}}()
        acc = 0
        step = i - 2
        j = 1
        while true
            acc += j
            if acc >= 10_000
                break
            end
            if acc >= 1_000 && acc % 100 > 9
                k, v = divrem(acc, 100)
                x[k] = push!(get(x, k, []), v)
            end
            j += step
        end
        tbl[i] = x
    end
    tbl
end

function find_closed_paths(max_nsides_polygon)
    paths::Vector{Vector{Int}} = []
    tbl = make_polynum_tbl(max_nsides_polygon)

    # example: (when max_nsides_polygon = 8)
    #   0b######000
    #     ||||||
    #     |||||+- triangle
    #     ||||+-- square
    #     |||+--- pentagonal
    #     ||+---- hexagonal
    #     |+----- heptagonal
    #     +------ octagonal
    stop_condition = (1 << (max_nsides_polygon + 1)) - 8

    function get_next_states((bits, path))
        states::Vector{Tuple{Int, Vector{Int}}} = []
        if bits == stop_condition && path[1] == path[end]
            push!(paths, path)
        else
            for i = 3:(max_nsides_polygon - 1)
                p_bit = 1 << i
                if bits & p_bit != 0
                    continue
                end
                next_tbl = tbl[i]
                if haskey(next_tbl, path[end])
                    for x in next_tbl[path[end]]
                        new_path = copy(path)
                        push!(new_path, x)
                        push!(states, (bits | p_bit, new_path))
                    end
                end
            end
        end
        states
    end

    # Search by DFS
    q::Vector{Tuple{Int, Vector{Int}}} = []
    for (k, vs) in tbl[max_nsides_polygon]
        for v in vs
            push!(q, (1 << max_nsides_polygon, [k, v]))
        end
    end
    while length(q) > 0
        state = pop!(q)
        for next_state in get_next_states(state)
            push!(q, next_state)
        end
    end

    paths
end

function solve_0061(max_nsides_polygon::Int = 8)
    function is_distinct_numbers(lst)
        tmp = Set{Int}()
        for i = 1:(length(lst) - 1)
            push!(tmp, lst[i] * 100 + lst[i + 1])
        end
        length(tmp) == (length(lst) - 1)
    end

    @assert (max_nsides_polygon > 3) "invalid parameter"

    cycles::Vector{Vector{Int}} = []
    for path in find_closed_paths(max_nsides_polygon)
        # All numbers in a cycle are different from each others
        if is_distinct_numbers(path)
            push!(cycles, path)
        end
    end

    # There exists only one cycle
    if length(cycles) == 1
        return sum(cycles[1][2:end]) * 101
    end
    @assert false "not reached"
end

end #module

using .Prob0061: solve_0061
export solve_0061
