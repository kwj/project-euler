
# project euler: problem 9

#=
  a = k(m^2 - n^2), b = k * 2mn, c = k(m^2 + n^2)  [m>n>0, gcd(m,n)=1, m+n is odd]

  abc = k^3 * (m^4 - n^4) * 2mn
  a + b + c = k * 2m(m+n) = 1000

  -> 'k' and 'm' are divisors to 500 (= 1000/2).
     'm+n' is a divisor to 500/m.
     m(m+n) <= 500 --> m <= isqrt(500), m+n <= 500/m
=#

module Prob0009

function solve_0009(perim::Int = 1_000)
    for m = 2:isqrt(perim ÷ 2)
        if (perim ÷ 2) % m != 0
            continue
        end
        x = m + 1 + (m % 2)    # x = m + n, x is odd number
        while x < 2m && x <= (perim ÷ 2) ÷ m
            if gcd(m, x) == 1 && (perim ÷ 2) ÷ m % x == 0
                k = (perim ÷ 2) ÷ m ÷ x
                n = x - m
                return (k ^ 3) * (m ^ 4 - n ^ 4) * 2 * m * n
            end
            x += 2
        end
    end

    return 0    # not reached when perimeter = 1000
end

end #module

using .Prob0009: solve_0009
export solve_0009
