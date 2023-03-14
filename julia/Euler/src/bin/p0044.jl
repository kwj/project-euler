
# project euler: problem 44

#=
  P(d) = P(k) - P(j) <==> d(3d-1) = k(3k-1) - j(3j-1) = (k-j)(3(k+j)-1)
    lhs: d(3d-1)
    rhs: (k-j) * (3(k+j)-1) = r1 * r2 [r1=k-j, r2=3(k+j)-1]
    0 < (k-j) < d, d % 3 == (k-j) % 3
=#

module Prob0044

import ..Util: divisors, is_pentagonal

function solve_0044()
    # get_divisors(n) returns divisors of n(3n-1) which meet the following requirements:
    #  - They are less than 'n'.
    #  - They are congruent to 'n' modulo 3.
    # note: 'n' and '3n-1' are relatively prime.
    get_divisors(n) = filter(x -> x < n && x % 3 == n % 3, vcat(divisors(n)[2:end], divisors(3n - 1)))
    pent(n) = (n * (3n - 1)) ÷ 2

    for d in Iterators.countfrom(4)
        lhs = d * (3d - 1)
        for r1 in get_divisors(d)
            r2 = lhs ÷ r1
            if r2 % 3 != 2
                continue
            end
            # if 'tmp' is even, tmp = 2k and j = k - r1.
            tmp = (r2 + 1) ÷ 3 + r1
            if iseven(tmp) != true
                continue
            end
            k = tmp ÷ 2
            j = k - r1
            if is_pentagonal(pent(k) + pent(j))
                return lhs ÷ 2
            end
        end
    end
end

end #module

using .Prob0044: solve_0044
export solve_0044
