
# project euler: problem 85

#=
  nCr = n! / ((n-r)! * r!)

      1  2       n-1  n
    +--+--+-- ... --+--+
   1|  |  |   ...   |  |
    +--+--+-- ... --+--+
   2|  |  |   ...   |  |
    +--+--+-- ... --+--+ num of horizontal lines = m + 1
   3|  |  |   ...   |  |
    +--+--+-- ... --+--+
    ....................
    +--+--+-- ... --+--+
   m|  |  |   ...   |  |
    +--+--+-- ... --+--+
      num of vertical lines = n + 1

  (m+1)C2 * (n+1)C2 = m(m+1)/2 * n(n+1)/2 (\approx) 2_000_000
    --> m(m+1)*n(n+1) ≈ 8_000_000
=#

module Prob0085

function get_diff(m::Int, target::Int)::Union{Tuple{Int, Int}, Nothing}
    lhs(m, n) = m * (m + 1) * n * (n + 1)

    n = isqrt(target ÷ (m * (m + 1))) - 1
    while lhs(m, n) < target
        n += 1
    end

    if m >= n
        return nothing
    end

    d1 = abs(target - lhs(m, n - 1))
    d2 = abs(target - lhs(m, n))
    if d1 < d2
        return d1, n - 1
    else
        return d2, n
    end
end

function solve_0085(target::Int = 2_000_000)
    new_target = target * 4
    min_diff = typemax(Int)
    answer = 0
    for m in Iterators.countfrom(1)
        if (tpl = get_diff(m, new_target)) === nothing
            break
        end
        if tpl[1] < min_diff
            answer = m * tpl[2]
            min_diff = tpl[1]
        end
    end
    answer
end

end #module

using .Prob0085: solve_0085
export solve_0085
