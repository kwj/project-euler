
# project euler: problem 95

module Prob0095

import ..Util: get_σ_tbl

function update_chain_tbl(tbl, elts, v)
    for idx in elts
        tbl[idx] = v
    end
end

function solve_0095(limit::Int = 1_000_000)
    spd_tbl = get_σ_tbl(1, limit)
    for i = 1:limit
        spd_tbl[i] -= i
    end

    chain_tbl = zeros(Int, limit)
    chain = Int[]
    max_length = 0
    for pos = 2:limit
        empty!(chain)
        while chain_tbl[pos] == 0
            push!(chain, pos)
            pos = spd_tbl[pos]
            if !(1 < pos <= limit) || pos in chain
                break
            end
        end

        if !(1 < pos <= limit) || chain_tbl[pos] != 0
            update_chain_tbl(chain_tbl, chain, -1)
        else
            i = 1
            while pos != chain[i]
                i += 1
            end
            len = length(chain) - i + 1
            update_chain_tbl(chain_tbl, chain[begin:i - 1], -1)
            update_chain_tbl(chain_tbl, chain[i:end], len)
            max_length = max(max_length, len)
        end
    end
    findfirst(x -> x == max_length, chain_tbl)
end

end #module

using .Prob0095: solve_0095
export solve_0095
