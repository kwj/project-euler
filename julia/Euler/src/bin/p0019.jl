
# project euler: problem 19

module Prob0019

function solve_0019()
    common_year = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    leap_year = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    # days per month (Jan 1901 - *Nov* 2000)
    days = repeat(vcat(repeat(common_year, 3), leap_year), 25)[1:end-1]

    # Jan 1, 1900 was Monday and assume that the day is the first day. (Monday is '1 mod 7 = 1')
    # And then, the year 1900 was a common year (365 days).
    # --> Jan 1, 1901 was Tuesday since (1 + 365) mod 7 = 2.
    #     Feb 1, 1901 was Firday since ((1 + 365) + 31) mod 7 = 5.
    #     ... and so on
    pushfirst!(days, 1 + 365)
    length(filter(x -> x % 7 == 0, accumulate(+, days)))
end

end #module

using .Prob0019: solve_0019
export solve_0019
