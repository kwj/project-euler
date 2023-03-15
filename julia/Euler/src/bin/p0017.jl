
# project euler: problem 17

module Prob0017

const words = Dict{Int, Int}([
    (1, length("one")),
    (2, length("two")),
    (3, length("three")),
    (4, length("four")),
    (5, length("five")),
    (6, length("six")),
    (7, length("seven")),
    (8, length("eight")),
    (9, length("nine")),
    (10, length("ten")),
    (11, length("eleven")),
    (12, length("twelve")),
    (13, length("thirteen")),
    (14, length("fourteen")),
    (15, length("fifteen")),
    (16, length("sixteen")),
    (17, length("seventeen")),
    (18, length("eighteen")),
    (19, length("nineteen")),
    (20, length("twenty")),
    (30, length("thirty")),
    (40, length("forty")),
    (50, length("fifty")),
    (60, length("sixty")),
    (70, length("seventy")),
    (80, length("eighty")),
    (90, length("ninety")),
    (0, 0)
])

function solve_0017(limit::Int = 1_000)
    acc = 0
    for n = 1:limit
        if n == 1_000
            # one thouthand (3 + 8)
            acc += 11
        elseif n < 20
            acc += words[n]
        elseif n < 100
            acc += words[n - (n % 10)] + words[n % 10]
        elseif n % 100 == 0
            # xxx hundred (length(xxx) + 7)
            acc += words[n ÷ 100] + 7
        elseif n % 100 < 20
            # xxx handred and ...
            acc += words[n ÷ 100 ] + 7 + 3 + words[n % 100]
        else
            # xxx handred and ...
            acc += words[n ÷ 100 ] + 7 + 3 + words[(n % 100) - (n % 10)] + words[n % 10]
        end
    end
    acc
end

end #module

using .Prob0017: solve_0017
export solve_0017
