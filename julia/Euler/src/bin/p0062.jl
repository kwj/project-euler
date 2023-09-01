
# project euler: problem 62

module Prob0062

function solve_0062(n_perms::Int = 5)
    make_key(n::Int) = mapfoldl(string, *, sort(digits(n)); init = "")

    tbl = Dict{String, Vector{Int}}()
    for n in Iterators.countfrom(1)
        cube = n ^ 3
        key = make_key(cube)
        if haskey(tbl, key)
            push!(tbl[key], n)
            if length(tbl[key]) == n_perms
                return tbl[key][1] ^ 3
            end
        else
            tbl[key] = [n]
        end
    end
end

end #module

using .Prob0062: solve_0062
export solve_0062
