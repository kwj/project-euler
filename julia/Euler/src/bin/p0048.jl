
# project euler: problem 48

module Prob0048

function solve_0048(exp::Int = 1_000)
    # Return the answer as string because it must be a ten digits.
    string(mod(sum(powermod(x, x, 10^10) for x = 1:exp if x % 10 != 0), 10^10), pad=10)
end

end #module

using .Prob0048: solve_0048
export solve_0048
