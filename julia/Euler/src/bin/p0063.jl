
# project euler: problem 63

#=
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    --> n - 1 <= n * log10(m) < n
    --> m < 10
   and
    --> (n - 1)/n <= log10(m)
    --> 10 ^ (n - 1)/n <= 10 ^ log10(m)
    --> log10(10 ^ (n - 1)/n) <= log10(m)
    --> 10 ^ (n - 1)/n <= m
=#

module Prob0063

function solve_0063()
    acc = 0
    cnt = 0
    m = 1
    n = 1
    while m < 10
        while 10 ^ ((n - 1) / n) <= m
            n += 1
            cnt += 1
        end
        m += 1
        acc += cnt
    end
    acc
end

end #module

using .Prob0063: solve_0063
export solve_0063
