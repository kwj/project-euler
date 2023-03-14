
# project euler: problem 97

module Prob0097

function solve_0097(n_digits::Int = 10)
    # Return the answer as string because it must be a ten digits.
    divisor = 10 ^ n_digits
    lpad(string((28433 * powermod(2, 7830457, divisor) + 1) % divisor), n_digits, "0")
end

end #module

using .Prob0097: solve_0097
export solve_0097
