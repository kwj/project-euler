
# project euler: problem 43

module Prob0043

function solve_0043()
    lst = [""]
    for d in [1, 1, 17, 13, 11, 7, 5, 3, 2, 1]
        lst = [x * s for x in "0123456789" for s in lst if x ∉ s && (length(x * s) < 3 || parse(Int, (x * s)[1:3]) % d == 0)]
    end
    sum(map(x -> parse(Int, x), lst))
end

end #module

using .Prob0043: solve_0043
export solve_0043
