
# project euler: problem 73

#=
  f(n): number of fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2
        --> sigma{i=1, ...,n}((i-1)//2 - i//3)
  g(n): number of irreducible fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2, gcd(a,b)=1

    The answer we should seek is g(12000).

  f(n) = sigma{k=1, ..., n}(g(n//k))
  -->
    g(n) = sigma{k=1, ..., n}μ(k)f(n//k)      [möbius inversion formula, μ(): möbius function]
         = sigma{k=1, ..., n}μ(k)sigma{j=1, ..., n//k}((j-1)//2 - j//3)
=#

module Prob0073

function make_möbius_tbl(limit)
    p_tbl = collect(1:limit)
    for i = 2:isqrt(limit)
        if p_tbl[i] == i
            k = i ^ 2
            for j = k:i:limit
                p_tbl[j] = i
            end
            for j = k:k:limit
                p_tbl[j] = 0
            end
        end
    end

    mu_tbl = zeros(Int, limit)
    mu_tbl[1] = 1
    for i = 2:limit
        if p_tbl[i] != 0
            mu_tbl[i] = -mu_tbl[i ÷ p_tbl[i]]
        end
    end
    mu_tbl
end

function f(x)
    sum(map((j) -> ((j - 1) ÷ 2) - (j ÷ 3), collect(1:x)))
end

function g(N)
    mu_tbl = make_möbius_tbl(N)
    sum(map((k) -> mu_tbl[k] * f(N ÷ k), collect(1:N)))
end

function solve_0073(limit::Int = 12_000)
    g(limit)
end

end #module

using .Prob0073: solve_0073
export solve_0073
