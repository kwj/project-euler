
# project euler: problem 2

#=
  f₁ = 1, f₂ = 2, f₃ = 3, f₄ = 5, f₅ = 8, f₆ = 13, f₇ = 21, f₈ = 34, f₉ = 55, ...

  Even numbers are present for every three items.
  Assume that k ≥ 7:
    f(k) = f(k-1) + f(k-2)
         = 2f(k-2) + f(k-3)
         = 2(f(k-3) + f(k-4)) + f(k-3)
         = 3f(k-3) + 2f(k-4)
         = 3f(k-3) + 2f(k-5) + 2f(k-6)
         = 4f(k-3) - f(k-3) + 2f(k-5) + 2f(k-6)
         = 4f(k-3) - (f(k-4) + f(k-5)) + 2f(k-5) + 2f(k-6)
         = 4f(k-3) - f(k-4) + f(k-5) + 2f(k-6)
         = 4f(k-3) - f(k-4) + (f(k-5) + f(k-6)) + f(k-6)
         = 4f(k-3) - f(k-4) + f(k-4) + f(k-6)
         = 4f(k-3) + f(k-6)
=#

module Prob0002

function solve_0002(limit = 4_000_000)
    fₙ = 8; fₙ₋₁ = 2
    acc = fₙ₋₁
    while fₙ <= limit
        acc += fₙ
        (fₙ, fₙ₋₁) = (4fₙ + fₙ₋₁, fₙ)
    end
    acc
end

end #module

using .Prob0002: solve_0002
export solve_0002
