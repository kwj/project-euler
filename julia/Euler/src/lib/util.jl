
module Util

import Primes: factor, primes

export is_palindrome, is_pandigital, is_pandigital_nz
export is_triangular, is_square, is_pentagonal, is_hexagonal
export divisors, proper_divisors
export get_σ_tbl, get_max_exp, undigits

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
                if n % p == 0
                    continue
                end
                result[q * n] = result[q] * result[n]
            end
            q *= p
        end
    end
    result
end

function get_max_exp(num; base)
    e = 0
    while num >= base
        num ÷= base
        e += 1
    end
    e
end

undigits(lst::Vector{T}; base::Integer = 10) where {T<:Integer} = undigits(T, lst; base = base)

function undigits(T::Type{<:Integer}, lst::Vector{U}; base::Integer = 10) where {U<:Integer}
    foldr((x, acc) -> acc * base + x, lst; init = zero(T))
end

end #module
