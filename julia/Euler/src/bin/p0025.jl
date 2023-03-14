
# project euler: problem 25

#=
  f₁ = 1, f₂ = 1, f₃ = 2, f₄ = 3, f₅ = 5, f₆ = 8, f₇ = 13, f₈ = 21, f₉ = 34, ...
=#

module Prob0025

function solve_0025(digits = 1_000)
    boundary = big(10) ^ (digits - 1)
    fₙ = big(1); fₙ₋₁ = big(1)
    idx = 2
    while fₙ < boundary
        idx += 1
        (fₙ, fₙ₋₁) = (fₙ + fₙ₋₁, fₙ)
    end
    idx
end

end #module

using .Prob0025: solve_0025
export solve_0025
