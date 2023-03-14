
# project euler: problem 45

#=
  H_{n} = T_{2n-1}
=#

module Prob0045

import ..Util: is_pentagonal

function solve_0045()
    for n in Iterators.countfrom(144)
        hex_num = n * (2n - 1)
        if is_pentagonal(hex_num)
            return hex_num
        end
    end
end

end #module

using .Prob0045: solve_0045
export solve_0045
