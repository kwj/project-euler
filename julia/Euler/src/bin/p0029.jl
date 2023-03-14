
# project euler: problem 29

module Prob0029

import ..Util: get_max_exp

#=
# version 1
function solve_0029(upper = 100)
    dup_tbl = falses(upper, upper)
    for x = 2:isqrt(upper)
        for y = 2:get_max_exp(upper, base=x)
            for z = 1:(y - 1)
                k = lcm(y, z) ÷ y
                dup_tbl[x^y, max(k, 2):k:((upper * z) ÷ y)] .= true
            end
        end
    end
    (upper - 1) ^ 2 - count(dup_tbl)
end

# version 2
function solve_0029(upper = 100)
    dup_dict = Dict{Int, Int}()
    acc = 0
    for x = 2:isqrt(upper)
        for y = 2:get_max_exp(upper, base=x)
            if get(dup_dict, x^y, 0) != 0
                continue
            end
            dups = zeros(Int, 1, upper)
            for z = 1:(y - 1)
                k = lcm(y, z) ÷ y
                dups[max(k, 2):k:((upper * z) ÷ y)] .= 1
            end
            dup_dict[x^y] = sum(dups)
            acc += sum(dups)
        end
    end
    (upper - 1) ^ 2 - acc
end
=#

function make_dupctr_tbl(upper)
    max_exp = get_max_exp(upper, base=2)
    dup_ctr = zeros(Int, 1, max_exp)

    dups = Array{Int, 1}(undef, upper)
    for x = 2:max_exp
        fill!(dups, 0)
        for y = 1:(x - 1)
            k = lcm(x, y) ÷ x
            dups[max(k, 2):k:((upper * y) ÷ x)] .= 1
        end
        dup_ctr[x] = sum(dups)
    end
    dup_ctr
end

function solve_0029(upper = 100)
    dup_ctr = make_dupctr_tbl(upper)
    base_limit = isqrt(upper)
    skip_flag = falses(base_limit)

    answer = (upper - 1) ^ 2
    for b = 2:base_limit
        if skip_flag[b] == true
            continue
        end
        for e = 2:get_max_exp(upper, base=b)
            answer -= dup_ctr[e]
            tmp = b ^ e
            if tmp <= base_limit
                skip_flag[tmp] = true
            end
        end
    end
    answer
end

end #module

using .Prob0029: solve_0029
export solve_0029
