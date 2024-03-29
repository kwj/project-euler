# project euler: problem 93

# It is very slow, however I wanted to use Reverse Polish notation and Python's fraction module.
#
#   Infix notation and Reverse Polish notation
#
#     case 1:
#        IN: ((x1 OP1 x2) OP2 x3) OP3 x4
#       RPN: x1 x2 OP1 x3 OP2 x4 OP3
#
#     case 2:
#        IN: (x1 OP1 (x2 OP2 x3)) OP3 x4
#       RPN: x1 x2 x3 OP2 OP1 x4 OP3
#
#     case 3:
#        IN: (x1 OP1 x2) OP2 (x3 OP3 x4)
#       RPN: x1 x2 OP1 x3 x4 OP3 OP2
#
#     # The following cases can be ignored since the following reasons
#     #   - they are symmetrical with the above cases
#     #   - numbers(x{n}) are provided as permutations
#     #   - operators(OP{n}) are provided as permutations with repetition
#
#     case 4:   (it is symmetrical with case 2)
#        IN: x1 OP1 ((x2 OP2 x3) OP3 x4)
#       RPN: x1 x2 x3 OP2 x4 OP3 OP1
#
#     case 5:   (it is symmetrical with case 1)
#        IN: x1 OP1 (x2 OP2 (x3 OP3 x4))
#       RPN: x1 x2 x3 x4 OP3 OP2 OP1

from collections import deque
from collections.abc import Callable, Generator
from fractions import Fraction
from itertools import permutations, product
from operator import add, mul, sub, truediv

from euler.lib.util import HeapQueue, flatten


def calc_rpn(
    rpn: list[Fraction | Callable[[Fraction, Fraction], Fraction]]
) -> Fraction:
    stack: deque[Fraction] = deque()
    for elt in rpn:
        if callable(elt):
            t2 = stack.pop()
            t1 = stack.pop()
            if t2 == 0:
                return Fraction(0)  # skip if zero divide occurs
            stack.append(elt(t1, t2))
        else:
            stack.append(elt)

    return stack[0]


def gen_num_tpl() -> Generator[tuple[Fraction, ...], None, None]:
    lst = []
    for i in range(1, 10):
        lst.append(Fraction(i))

    for x in (
        tpl
        for tpl in permutations(lst, r=4)
        if tpl[0] < tpl[1] and tpl[1] < tpl[2] and tpl[2] < tpl[3]
    ):
        yield x


def make_result(
    num_tpl: tuple[Fraction, ...],
    op_tpl_lst: list[tuple[Callable[[Fraction, Fraction], Fraction], ...]],
) -> list[int]:
    # case 1,2 and 3
    result = (
        [
            calc_rpn([x1, x2, op1, x3, op2, x4, op3]),
            calc_rpn([x1, x2, x3, op2, op1, x4, op3]),
            calc_rpn([x1, x2, op1, x3, x4, op3, op2]),
        ]
        for x1, x2, x3, x4 in permutations(num_tpl)
        for op1, op2, op3 in op_tpl_lst
    )

    return sorted(
        list(
            set(
                frac.numerator
                for frac in flatten(result)
                if frac.denominator == 1 and frac.numerator > 0
            )
        )
    )


def get_consecutive_length(lst: list[int]) -> int:
    for i, elt in enumerate(lst):
        if i + 1 != elt:
            return i

    assert False, 'unreachable!'


def compute() -> str:
    op_tpl_lst = list(product((add, sub, mul, truediv), repeat=3))
    pq = HeapQueue(desc=True)
    for num_tpl in gen_num_tpl():
        pq.insert((get_consecutive_length(make_result(num_tpl, op_tpl_lst)), num_tpl))

    _, answer = pq.peek()
    return ''.join(str(frac.numerator) for frac in answer)


def solve() -> str:
    return compute()
