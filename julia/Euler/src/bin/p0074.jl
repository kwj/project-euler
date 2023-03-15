
# project euler: problem 74

module Prob0074

function fact_sum(n)
    tbl = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
    if n == 0
        return tbl[0]
    end

    acc = 0
    while n > 0
        acc += tbl[(n % 10) + 1]
        n ÷= 10
    end
    acc
end

function solve_0074(limit::Int = 1_000_000, thr::Int = 60)
    chain_tbl = zeros(Int, limit)
    footprints = Array{Int}(undef, 0)
    cnt = 0
    for n = 1:(limit - 1)
        empty!(footprints)
        steps = 0
        pos = n
        while (pos in footprints) == false
            if (pos < limit) && (chain_tbl[pos] != 0)
                steps += chain_tbl[pos]
                break
            end
            push!(footprints, pos)
            pos = fact_sum(pos)
            steps += 1
        end

        if steps - length(footprints) < thr <= steps
            cnt += 1
        end
        for pos in footprints
            if pos < limit
                chain_tbl[pos] = steps
            end
            steps -= 1
        end
    end
    cnt
end

end #module

using .Prob0074: solve_0074
export solve_0074
