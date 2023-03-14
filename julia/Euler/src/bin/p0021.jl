
# project euler: problem 21

module Prob0021

import ..Util: get_σ_tbl

function solve_0021(limit = 10_000)
    upper = limit - 1
    d_tbl = get_σ_tbl(1, upper)
    for x = 1:upper
        d_tbl[x] -= x
    end

    sum(x + d_tbl[x] for x = 2:upper if x > d_tbl[x] && d_tbl[d_tbl[x]] == x)
end

end #module

using .Prob0021: solve_0021
export solve_0021
