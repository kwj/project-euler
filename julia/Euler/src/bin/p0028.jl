
# project euler: problem 28

#=
   21 22 23 24 25
   20  7  8  9 10
   19  6  1  2 11
   18  5  4  3 12
   17 16 15 14 13
          |  |  |  |
       (n=0, 1, 2, 3, ...)

   the upper right number is:
     1    [n=0]
     (2n+1)**2    [n=>1]

   so, the sum of numbers in the four corners is:
     (2n+1)**2 + ((2n+1)**2 - 2n) + ((2n+1)**2 - 4n) + ((2n+1)**2 - 6n)
       = 16n**2 + 4n + 4   [n>=1]

   Answer: 1 + sum_{n=1}^{(1001-1)/2} (16n**2 + 4n + 4)
=#

module Prob0028

function solve_0028(len::Int = 1_001)
    result = 1
    for n = 1:((len - 1) ÷ 2)
        result += 16n^2 + 4n + 4
    end
    result
end

end #module

using .Prob0028: solve_0028
export solve_0028
