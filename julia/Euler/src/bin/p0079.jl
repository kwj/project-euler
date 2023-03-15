
# project euler: problem 79

#=
  If you look at the log file, you can assume the answer with pen and paper :).

  We'll need the following files to run this program.
    - https://projecteuler.net/project/resources/p079_keylog.txt

  I saw the Wikipedia page about topological sorting.
    https://en.wikipedia.org/wiki/Topological_sorting

  Note: This implementation finds only one topological sort not all.
=#

module Prob0079

import ..Util: undigits

function dfs(graph, perm, v)
    function visit(temp::Vector{Char}, visited::Vector{Char}, node::Char)
        if node in temp
            @assert false "cycle path is found"
        end
        if node in visited
            return visited
        end
        if haskey(graph, node) == true
            acc = visited
            for v in graph[node]
                acc = visit(vcat([node], temp), acc, v)
            end
            return vcat([node], acc)
        else
            return [node]
        end
    end

    visit(Vector{Char}(), perm, v)
end

function solve_0079(fname::String = "p079_keylog.txt")
    graph = Dict{Char, Set{Char}}()
    for code in readlines(joinpath((@__DIR__), "../../assets", fname))
        v1 = get!(graph, code[1], Set{Char}())
        push!(v1, code[2], code[3])
        graph[code[1]] = v1
        v2 = get!(graph, code[2], Set{Char}())
        push!(v2, code[3])
        graph[code[2]] = v2
    end

    acc = Vector{Char}()
    for v in keys(graph)
        acc = dfs(graph, acc, v)
    end
    undigits(reverse(map((x) -> parse(Int, x), acc)))
end

end #module

using .Prob0079: solve_0079
export solve_0079
