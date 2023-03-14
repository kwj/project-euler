
# project euler: problem 71

#=
  Farey sequence

  2/5, 3/7
    -> 2/5, (2+3)/(5+7), 3/7
    -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), 3/7
    -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), (2+3+3+3)/(5+7+7+7), 3/7
     ...
    -> 2/5, ..., (2+3x)/(5+7x), 3/7

      5+7x <= 1_000_000
=#

module Prob0071

function solve_0071(limit::Int = 1_000_000)
    2 + 3 * ((limit - 5) ÷ 7)
end

end #module

using .Prob0071: solve_0071
export solve_0071
