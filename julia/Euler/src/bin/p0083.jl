
# project euler: problem 83

#=
  I used Dijkstra's algorithm to solve.
    https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
=#

module Prob0083

import DataStructures: PriorityQueue, enqueue!, dequeue_pair!, peek, isempty

function make_neighbor_tbl(nrow, ncol)
    tbl = Array{Vector{Tuple{Int, Int}}}(undef, nrow, ncol)
    for r = 1:nrow, c = 1:ncol
        tbl[r, c] = filter((tpl) -> tpl[1] in 1:nrow && tpl[2] in 1:ncol, [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)])
    end
    tbl
end

function solve_0083(fname::String = "p083_matrix.txt")
    data = vcat(([parse(Int, x) for x in split(l, ",")]' for l in readlines(joinpath((@__DIR__), "../../assets", fname)))...)

    nbr_tbl = make_neighbor_tbl(size(data, 1), size(data, 2))
    dist_tbl = fill(typemax(Int), (size(data, 1), size(data, 2)))
    dist_tbl[1, 1] = data[1, 1]

    pq = PriorityQueue{Tuple{Int, Int}, Int}()
    enqueue!(pq, (1, 1) => dist_tbl[1, 1])

    while isempty(pq) == false
        (i, j), d = dequeue_pair!(pq)
        for (x, y) in nbr_tbl[i, j]
            if (new_d = d + data[x, y]) < dist_tbl[x, y]
                dist_tbl[x, y] = new_d
                enqueue!(pq, (x, y) => new_d)
            end
        end
    end
    dist_tbl[end, end]
end

end #module

using .Prob0083: solve_0083
export solve_0083
