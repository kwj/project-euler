
# project euler: problem 39

#=
  assume that a <= b < c, a + b + c = p ==> a < p/3

  a^2 + b^2 = (p - a - b)^2
  => a^2 + b^2 = p^2 -2ap - 2bp + a^2 + 2ab + b^2
  => p^2 -2ap - 2bp + 2ab = 0
  => 2bp - 2ab = p^2 - 2ap
  => 2b(p - a) = p^2 - 2ap
  => b = (p^2 - 2ap) / 2(p - a)

     a  b  p  (E:even, O:odd)
   -----------
     E  E  E
     E  E  O  ==> NG (contradiction, c is even because c^2 = a^2 + b^2 is even.)
     E  O  E
     E  O  O  ==> NG (contradiction, c is odd because c^2 = a^2 + b^2 is odd.)
     O  E  E
     O  E  O  ==> NG (contradiction, c is odd because c^2 = a^2 + b^2 is odd.)
     O  O  E
     O  O  O  ==> NG (contradiction, c is even because c^2 = a^2 + b^2 is even.)

    'p' is always EVEN.
=#

module Prob0039

function solve_0039(limit::Int = 1_000)
    function check_pair(p, a)
        (p * p - 2 * a * p) % (2 * (p - a)) == 0
    end

    result = Array{Tuple{Int, Int}}(undef, 0)
    for p = 2:2:limit
        lst = Array{Tuple{Int, Int, Int}}(undef, 0)
        for a = 1:((p - 1) ÷ 3)
            if check_pair(p, a) == true
                b = (p * p - 2 * a * p) ÷ (2 * (p - a))
                push!(lst, (a, b, p - a - b))
            end
        end
        if length(lst) > 0
            push!(result, (length(lst), p))
        end
    end

    sort(result, by = x -> x[1], rev = true)[1][2]
end

end #module

using .Prob0039: solve_0039
export solve_0039
