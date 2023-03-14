
# project euler: problem 26

module Prob0026

function find_cycle_length(n, d)
    rems = Dict{Int, Int}()
    cnt = 0
    while true
        if n == 0
            return 0
        end
        if haskey(rems, n) == true
            return (cnt - rems[n])
        end

        rems[n] = cnt
        n = (n * 10) % d
        cnt += 1
    end
end

function solve_0026(upper = 1_000)
    upper -= 1
    (max_cycle, idx) = (0, 0)

    for i = upper:-1:1
        if i <= max_cycle
            break
        end
        cycle_len = find_cycle_length(1, i)
        if cycle_len > max_cycle
            (max_cycle, idx) = (cycle_len, i)
        end
    end
    idx
end

end #module

using .Prob0026: solve_0026
export solve_0026
