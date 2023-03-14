
# project euler: problem 75

#=
  Pythagorean triple

    a = k * (m^2 - n^2), b = k * 2mn, c = k * (m^2 + n^2)
      where m > n > 0, gcd(m, n) = 1

    perimeter L = k * (2m^2 + 2mn)
                = k * 2m(m + n)

    2m(m + n) = L/k
      -->
    2m^2 < 2m(m + n) = L/k
      <-->
    m^2 < L/2k

    'm' is maximized when k=1
      max(m) < sqrt(L/2)
=#

module Prob0075

function solve_0075(L::Int = 1_500_000)
    limit = isqrt(L ÷ 2)
    counter = zeros(Int, L)

    for m = 2:limit
        for n = (1 + (m % 2)):2:m
            if gcd(m, n) == 1
                if (perimeter =  2 * m * (m + n)) > L
                    break
                end
                for i = perimeter:perimeter:L
                    counter[i] += 1
                end
            end
        end
    end
    count((x) -> x == 1, counter)
end

end #module

using .Prob0075: solve_0075
export solve_0075
