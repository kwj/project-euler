
# project euler: problem 23

module Prob0023

import ..Util: get_σ_tbl

function solve_0023(upper = 28_123)
    d_tbl = get_σ_tbl(1, upper)
    for x = 1:upper
        d_tbl[x] -= x
    end

    abndnt_flag = [i < d_tbl[i] for i in 1:upper]
    abndnt_lst = Int[]
    acc = 0
    for i = 1:upper
        if i % 2 == 0 && abndnt_flag[i ÷ 2] == true
            push!(abndnt_lst, i ÷ 2)
        end
        if any(abndnt_flag[i - x] for x in abndnt_lst)
            continue
        end
        acc += i
    end
    acc
end

end #module

using .Prob0023: solve_0023
export solve_0023
