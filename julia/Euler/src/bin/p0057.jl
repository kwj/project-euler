
# project euler: problem 57

#=
  use recurrence relation:
    sqrt(2) = 1 + sqrt(2) - 1
            = 1 + 1 / ( 1 / (sqrt(2) - 1) )
            = 1 + 1 / ( (sqrt(2) + 1) / (2 - 1) )
            = 1 + 1 / (1 + sqrt(2))
    -->
    a{1} = 1 + 1/2
    a{n} = 1 + 1/(1 + a{n-1})    [n>1]

  assume that b{n}/c{n} = a{n}
    b{1}/c{1} = 1 + 1/2 = 3/2
    b{n}/c{n} = 1 + 1/(1 + b{n-1}/c{n-1})
              = 1 + 1/((c{n-1) + b{n-1})/c{n-1})
              = 1 + c{n-1}/(c{n-1) + b{n-1})
              = (c{n-1) + b{n-1} + c{n-1))/(c{n-1) + b{n-1})
              = (2 * c{n-1} + b{n-1}) / (c{n-1) + b{n-1})
=#

module Prob0057

function solve_0057(num::Int = 1_000)
    answer = 0
    b, c = BigInt(1), BigInt(1)
    for _ = 1:num
        b, c = 2c + b, c + b
        if ndigits(b) > ndigits(c)
            answer += 1
        end
    end
    answer
end

end #module

using .Prob0057: solve_0057
export solve_0057
