# project euler: problem 17

words = dict(
    [
        (1, len('one')),
        (2, len('two')),
        (3, len('three')),
        (4, len('four')),
        (5, len('five')),
        (6, len('six')),
        (7, len('seven')),
        (8, len('eight')),
        (9, len('nine')),
        (10, len('ten')),
        (11, len('eleven')),
        (12, len('twelve')),
        (13, len('thirteen')),
        (14, len('fourteen')),
        (15, len('fifteen')),
        (16, len('sixteen')),
        (17, len('seventeen')),
        (18, len('eighteen')),
        (19, len('nineteen')),
        (20, len('twenty')),
        (30, len('thirty')),
        (40, len('forty')),
        (50, len('fifty')),
        (60, len('sixty')),
        (70, len('seventy')),
        (80, len('eighty')),
        (90, len('ninety')),
        (0, 0),  # special data
    ]
)


def compute(limit: int) -> str:
    cnt = 0
    for n in range(1, limit + 1):
        if n == 1000:
            # one thouthand (3 + 8)
            cnt += 11
        elif n < 20:
            cnt += words[n]
        elif n < 100:
            cnt += words[n - (n % 10)] + words[n % 10]
        elif n % 100 == 0:
            # xxx hundred (len(xxx) + 7)
            cnt += words[n // 100] + 7
        elif n % 100 < 20:
            # xxx hundred and ...
            cnt += words[n // 100] + 7 + 3 + words[n % 100]
        else:
            # xxx hundres and ...
            cnt += words[n // 100] + 7 + 3 + words[(n % 100) - (n % 10)] + words[n % 10]

    return str(cnt)


def solve() -> str:
    return compute(1000)
