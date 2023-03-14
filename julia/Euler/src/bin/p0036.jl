
# project euler: problem 36

module Prob0036

import ..Util: is_palindrome

function solve_0036(limit = 1_000_000)
    function check_palindrome(n)
        is_palindrome(n) == true && is_palindrome(n, base=2) == true
    end

    sum(i for i = 1:2:(limit - 1) if check_palindrome(i) == true)
end

end #module

using .Prob0036: solve_0036
export solve_0036
