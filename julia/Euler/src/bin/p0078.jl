
# project euler: problem 78

#=
  p(5) = 7
  p(10) = 42
  p(50) = 204226
  p(100) = 190569292
  p(200) = 3972999029388
  p(500) = 2300165032574323995027
  p(1000) = 24061467864032622473692149727991
    ...

  I needed to find another way instead of dynamic programming.
  Unfortunately, I gave up trying to solve it on my own at last.

  I saw following pages.

    https://en.wikipedia.org/wiki/Partition_(number_theory)
    https://en.wikipedia.org/wiki/Partition_function_(number_theory)
    https://en.wikipedia.org/wiki/Pentagonal_number_theorem

    p(n) = Sigma{k ∈ Z/{0}} (-1)^(k+1) * p(n - k(3k-1)/2)
         = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-12) + p(n-15) - p(n-22) - ...

      [p(0) = 1, p(k) = 0 when k < 0]

  I consider only value of 'mod 1_000_000' because the problem is divisible by one million or not.
=#

module Prob0078

#=
# another version
function solve_0078(denom::Int = 1_000_000)
    p = [1]
    n = 1
    while true
        sign = 1
        k = 1
        rem = 0
        while true
            if (n₁ = n - k * (3k - 1) ÷ 2) >= 0
                rem = mod(rem + sign * p[n₁ + 1], denom)
            end
            if (n₂ = n - k * (3k + 1) ÷ 2) >= 0
                rem = mod(rem + sign * p[n₂ + 1], denom)
            end

            if n₁ < 0 || n₂ < 0
                break
            end
            sign = -sign
            k += 1
        end
        if rem == 0
            break
        end
        push!(p, rem)
        n += 1
    end
    n
end
=#

function gp_generator(c)
    # Generalized pentagonal numbers
    #        0   1   2   5   7   12   15   22   26   35   40   51   57   70   77   92   100   117  ...
    # diff:    1   1   3   2   5    3    7    4    9    5    11   6    13   7    15    8    17
    # g/s      g   s   g   s   g    s    g    s    g    s     g   s     g   s     g    s     g
    #   [g: gap, s: step]
    gap = 1
    step = 1
    acc = 0
    while true
        acc += gap
        put!(c, acc)
        gap += 2

        acc += step
        put!(c, acc)
        step += 1
    end
end

function solve_0078(denom::Int = 1_000_000)
    # generalized pentagonal numbers: gp[1] = 1, gp[2] = 2, gp[3] = 5, gp[4] = 7, ...
    gp_gen = Channel(gp_generator)
    gp = Vector{Int}(undef, 0)
    push!(gp, take!(gp_gen))

    # number of partitions of n: P[n] = p[n + 1]
    # P[0] = p[1] = 1
    p = Vector{Int}(undef, 0)
    push!(p, 1)

    n = 1
    while true
        if n > gp[end]
            push!(gp, take!(gp_gen))
        end

        rem = 0
        for (i, x) in pairs(gp)
            if x > n
                break
            end
            rem = mod(rem + ((mod1(i, 4) <= 2) ? p[n - x + 1] : -p[n - x + 1]), denom)
        end
        if rem == 0
            break
        end
        push!(p, rem)
        n += 1
    end
    n
end

end #module

using .Prob0078: solve_0078
export solve_0078
