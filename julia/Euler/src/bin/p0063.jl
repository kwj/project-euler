
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
    cnt = 0
    for m = 1:9
        for n in Iterators.countfrom(1)
            if (big(10) ^ (n - 1)) ^ (1 / n) > m
                break
            end
            cnt += 1
        end
    end
    cnt
end

end #module

using .Prob0063: solve_0063
export solve_0063
