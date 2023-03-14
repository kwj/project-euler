
# project euler: problem 53

#=
  Pascal's triangle
    C(n,r) = C(n-1,r-1) + C(n-1,r)
    C(n,0) = C(n,n) = 1

  Central binomial coefficient
    https://en.wikipedia.org/wiki/Central_binomial_coefficient
    1, 2, 6, 20, 70, 252, 924, 3432, 12870, 48620, ...; (sequence A000984 in the OEIS)

    C(2n,n) = C(2(n-1),n-1) * (3 + (n-2)/n)
            = C(2(n-1),n-1) * (4n-2)/n  [n >= 1]
=#

module Prob0053

#=
# old version: #1
function solve_0053(N::Integer = 100, thr::Integer = 1_000_000)
    @assert N >= 1 "invalid parameter"
    data = ones(Int, N + 1)
    answer = (thr > 0) ? 0 : 2N
    for start = 2:N
        stop = ((start + 1) ÷ 2) + 1
        for i = start:-1:stop
            data[i] = data[i] + data[i - 1]
            if data[i] > thr
                answer += i - (start - i) - 1
                break
            end
        end
        if isodd(start) == true
            data[stop - 1] = data[stop]
        end
    end
    answer
end

# old version: #2
function solve_0053(N::Integer = 100, thr::Integer = 1_000_000)
    @assert N >= 1 "invalid parameter"
    if thr < 1
        return ((1 + (N + 1)) * (N + 1) ÷ 2) - 1
    end

    # start from C(N, 1)
    # 1) move right while 'c' <= 'thr'
    # 2) When 'c' > 'thr', count up target elements then go to previous row (move upper-right).
    n = x = N
    c = r = 1
    answer = 0
    while r <= n ÷ 2
        c = c * x ÷ r
        if c > thr
            answer += n - 2r + 1
            c = c * r ÷ n
            n -= 1
        else
            r += 1
        end
        x -= 1
    end
    answer
end
=#

function solve_0053(N::Integer = 100, thr::Integer = 1_000_000)
    @assert N >= 1 "invalid parameter"

    # find the first central binomial coefficient C(2k, k) which is larger than 'thr'.
    c = 2
    x = 1
    while c <= thr
        x += 1
        c = c * (4x - 2) ÷ x
    end

    # the start position 'c' = C(n, r) is C(2k-1, k-1). And then set x = (n - r + 1).
    c ÷= 2
    r = x - 1
    x += 1

    answer = 0
    for n = (r + x - 1):N
        # move left until 'c' <= thr
        while c > thr
            c = c * r ÷ x
            r -= 1
            x += 1
        end
        answer += n - 2r - 1

        # go to next row (down-left)
        c = c * (n + 1) ÷ x
        x += 1
    end
    answer
end

end #module

using .Prob0053: solve_0053
export solve_0053
