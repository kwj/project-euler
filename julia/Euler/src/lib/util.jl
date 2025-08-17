
module Util

import Primes: factor, primes

export is_palindrome, is_pandigital, is_pandigital_nz
export is_triangular, is_square, is_pentagonal, is_hexagonal
export divisors, proper_divisors, phi
export get_σ_tbl, σ₁, get_max_exp, undigits
export with_replacement_permutations

function is_palindrome(num; base=10)
    x = num
    acc = 0
    while x > 0
        acc = acc * base + (x % base)
        x ÷= base
    end
    acc == num
end

function is_pandigital(num)
    function mk_bits(n)
        bits = 0
        while n > 0
            bits |= (1 << (n % 10))
            n ÷= 10
        end
        bits
    end

    mk_bits(num) == ((1 << ndigits(num)) - 1)
end

function is_pandigital_nz(num)
    function check_zero(n)
        while n > 0
            if n % 10 == 0
                return false
            end
            n ÷= 10
        end
        return true
    end

    check_zero(num) && is_pandigital(num * 10)
end

function is_triangular(num)
    tmp = 8 * num + 1
    tmp_sqrt = isqrt(tmp)

    tmp_sqrt * tmp_sqrt == tmp && tmp_sqrt % 2 == 1
end

function is_square(num)
    n_sqrt = isqrt(num)

    n_sqrt * n_sqrt == num
end

function is_pentagonal(num)
    tmp = 24 * num + 1
    tmp_sqrt = isqrt(tmp)

    tmp_sqrt * tmp_sqrt == tmp && tmp_sqrt % 6 == 5
end

function is_hexagonal(num)
    tmp = 8 * num + 1
    tmp_sqrt = isqrt(tmp)

    tmp_sqrt * tmp_sqrt == tmp && tmp_sqrt % 4 == 3
end

function divisors(num)
    @assert num > 0 "Range Error: parameter must be positive number [$num]"
    lst = [1]
    for (b, e) in factor(num)
        acc_lst = []
        for m = map(x -> b ^ x, 1:e)
            append!(acc_lst, map(x -> x * m, lst))
        end
        append!(lst, acc_lst)
    end
    sort(lst)
end

function proper_divisors(num)
    @assert num > 1 "Range Error"
    divisors(num)[1:end-1]
end

function phi(n)
    ret = n

    if n % 2 == 0
        ret -= div(ret, 2)
        while n % 2 == 0
            n = div(n, 2)
        end
    end

    i = 3
    while i * i <= n
        if n % i == 0
            ret -= div(ret, i)
            while n % i == 0
                n = div(n, i)
            end
        end

        i += 2
    end

    if n > 1
        ret -= div(ret, n)
    end

    ret
end

# https://en.wikipedia.org/wiki/Divisor_function
function get_σ_tbl(z, upper)
    p_lst = primes(upper)
    result = ones(Int, upper)

    for p in p_lst
        q = p
        x = 0
        while q <= upper
            result[q] += (x += q ^ z)
            q *= p
        end
    end

    for p in p_lst
        q = p
        while q <= upper
            for n in 2:upper ÷ q
                if result[n] == 1 || n % p == 0
                    continue
                end
                result[q * n] = result[q] * result[n]
            end
            q *= p
        end
    end
    result
end

function σ₁(n)
    acc = 0
    k = 1
    while k <= n
        x = div(n, k)
        y = div(n, x)

        if k != y
            # arithmetic series
            # sum(k, k+1, ..., y) = div((k + y) * (y - k + 1), 2)
            acc += x * div((k + y) * (y - k + 1), 2)
        else
            # Actually, it's the same even without branching.
            #   div((k + y) * (y - k + 1), 2)
            #     = div((y + y) * (y - y + 1), 2)
            #     = div(2y * 1, 2)
            #     = y
            acc += x * y
        end

        k = y + 1
    end

    acc
end

function get_max_exp(num; base)
    e = 0
    while num >= base
        num ÷= base
        e += 1
    end
    e
end

undigits(lst::AbstractVector{T}; base::Integer = 10) where {T<:Integer} = undigits(T, lst; base = base)

function undigits(T::Type{<:Integer}, lst::AbstractVector{U}; base::Integer = 10) where {U<:Integer}
    foldr((x, acc) -> acc * base + x, lst; init = zero(T))
end

struct WithReplacementPermutations{T}
    a::T
    t::Integer
end

with_replacement_permutations(a, t::Integer) = WithReplacementPermutations(a, t)

function Base.iterate(p::WithReplacementPermutations, st = nothing)
    if isnothing(st)
        length(p.a) == 0 && p.t > 0 && return nothing

        return ([p.a[1] for _ in 1:p.t], ones(Int, p.t))
    else
        for k = p.t:-1:1
            st[k] == length(p.a) && continue

            result = Array{eltype(p.a)}(undef, p.t)
            for j = 1:k
                result[j] = p.a[st[j]]
            end

            st[k] += 1
            idx = st[k]
            result[k] = p.a[idx]
            for j = (k + 1):p.t
                st[j] = 1
                result[j] = p.a[1]
            end

            return (result, st)
        end

        return nothing
    end
end

function Base.length(p::WithReplacementPermutations)
    length(p.a) < p.t && return 0
    return Int(length(p.a) ^ p.t)
end

Base.eltype(p::WithReplacementPermutations) = Vector{eltype{p.a}}

Base.IteratorSize(p::WithReplacementPermutations) = Base.HasLength()

end #module
