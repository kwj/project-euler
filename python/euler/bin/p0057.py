# project euler: problem 57

#  use recurrence relation:
#    sqrt(2) = 1 + sqrt(2) - 1
#            = 1 + 1 / ( 1 / (sqrt(2) - 1) )
#            = 1 + 1 / ( (sqrt(2) + 1) / (2 - 1) )
#            = 1 + 1 / (1 + sqrt(2))
#    -->
#    a{1} = 1 + 1/2
#    a{n} = 1 + 1/(1 + a{n-1})    [n>1]
#
#  assume that b{n}/c{n} = a{n}
#    b{1}/c{1} = 1 + 1/2 = 3/2
#    b{n}/c{n} = 1 + 1/(1 + b{n-1}/c{n-1})
#              = 1 + 1/((c{n-1) + b{n-1})/c{n-1})
#              = 1 + c{n-1}/(c{n-1) + b{n-1})
#              = (c{n-1) + b{n-1} + c{n-1))/(c{n-1) + b{n-1})
#              = (2 * c{n-1} + b{n-1}) / (c{n-1) + b{n-1})

from euler.lib.util import num_of_digits


def compute(num: int) -> str:
    answer = 0
    b, c = 1, 1
    for _ in range(num):
        b, c = 2 * c + b, c + b
        if num_of_digits(b) > num_of_digits(c):
            answer += 1

    return str(answer)


def solve() -> str:
    return compute(1_000)
