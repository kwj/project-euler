
# project euler: problem 68

#=

       <e1>
          a2
            \
            a1
           /  \
         a5---a3-a4 <e2>
         /
       a6
      <e3>

       <e1>
          a2
            \   <e2>
            a1  a4
           /  \ /
         a9   a3
         /\   /
      a10 a7-a5-a6 <e3>
     <e5>   \
             a8
             <e4>

----
function solve_with_permutations()
    result = Array{Int}(undef, 0)
    for (a₂, a₁, a₃, a₄, a₅, a₆, a₇, a₈, a₉, a₁₀) in permutations(1:10)
        if a₂ > a₄ || a₂ > a₆ || a₂ > a₈ || a₂ > a₁₀
            continue
        end
        if a₄ != 10 && a₆ != 10 && a₈ != 10 && a₁₀ != 10
            continue
        end

        tmp = [[a₂, a₁, a₃], [a₄, a₃, a₅], [a₆, a₅, a₇], [a₈, a₇, a₉], [a₁₀, a₉, a₁]]
        if length(Set(sum.(tmp))) == 1
                push!(result, parse(Int64, foldl((acc, s) -> acc * s, map.(string, Iterators.flatten(tmp)); init = "")))
        end
    end
    sort(result, rev=true)[1]
end
=#

module Prob0068

function is_valid(x, y, num_bittbl, n_gon)
    (0 < x <= n_gon * 2 && 0 < y <= n_gon * 2) && x != y && (((1 << x) | (1 << y)) & num_bittbl) == 0
end

function dfs(n_gon, idx, num_bittbl, r, e_weight, result)
    # num_bittbl: 0x11111111110
    #               ^        ^
    #               10  ...  1
    if idx == (n_gon * 2 - 1)
        tmp = e_weight - r[1] - r[idx]
        if (0 < tmp <= n_gon * 2) && (1 << tmp) & num_bittbl == 0
            r[idx + 1] = tmp
            if r[2] == minimum(r[2:2:(n_gon * 2)])
                s = ""
                while idx > 0
                    s = string(r[idx + 1]) * string(r[idx]) * string(r[idx + 2]) * s
                    idx -= 2
                end
                push!(result, s)
            end
        end
        return
    end

    for out_node = 1:(n_gon * 2)
        in_node = e_weight - r[idx] - out_node
        if is_valid(out_node, in_node, num_bittbl, n_gon) == false
            continue
        end
        r[idx + 1] = out_node
        r[idx + 2] = in_node
        dfs(n_gon, idx + 2, ((1 << out_node) | (1 << in_node)) | num_bittbl, r, e_weight, result)
    end
    return
end

function solve_by_backtracking(n_gon)
    ring = Array{Int}(undef, n_gon * 2 + 1)
    result = Array{String}(undef, 0)

    # Example: if e-gon is 5, minimum: 1 + 2 + 10 = 13 and maximum: 1 + 9 + 10 = 20
    for e_weight = (1 + 2 + (n_gon * 2)):(1 + (n_gon * 2 - 1) + (n_gon * 2))
        for n = 1:(n_gon * 2)
            ring[1] = ring[end] = n
            dfs(n_gon, 1, (1 << n), ring, e_weight, result)
        end
    end

    if n_gon == 5
        result = filter(s -> length(s) == 16, result)
    end
    parse(Int64, maximum(result))
end

function solve_0068(n_gon::Int = 5)
    solve_by_backtracking(n_gon)
end

end #module

using .Prob0068: solve_0068
export solve_0068
