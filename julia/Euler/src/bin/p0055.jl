
# project euler: problem 55

module Prob0055

function is_rychrel(n)
    tmp = parse(BigInt, reverse(string(n)))
    for _ = 1:50
        n += tmp
        tmp = parse(BigInt, reverse(string(n)))
        if n == tmp
            return false
        end
    end
    return true
end

function solve_0055(upper::Int = 10_000)
    count(is_rychrel, BigInt(1):(upper -1))
end

end #module

using .Prob0055: solve_0055
export solve_0055
