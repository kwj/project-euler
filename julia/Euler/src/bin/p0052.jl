
# project euler: problem 52

module Prob0052

function check_num(num)
    make_key(n) = sort(digits(n))

    key = make_key(num)
    for mult = 2:6
        if key != make_key(num * mult)
            return false
        end
    end
    return true
end

function solve_0052()
    first(n for exp in Iterators.countfrom(6) for n = (10^(exp-1)):(10^exp ÷ 6) if check_num(n) == true)
end

end #module

using .Prob0052: solve_0052
export solve_0052
