
# project euler: problem 63

#=
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    --> n - 1 <= n * log10(m) < n
    --> m < 10
   and
    --> (n - 1)/n <= log10(m)
    --> (n - 1)/n <= log10(m)
    --> n/n - (n -1)/n >= 1 - log10(m)
    --> 1/n >= 1 - log10(m)
    --> 1/(1 - log10(m)) >= n
=#

module Prob0063

function solve_0063()
    sum(floor(Int, 1 / (1 - log10(m))) for m in 1:9)
end

end #module

using .Prob0063: solve_0063
export solve_0063
