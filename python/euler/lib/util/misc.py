
# Returns the largest exponent, e, for which base^e does not exceed num.
def get_max_exp(num, /, base):
    e = 0
    while num >= base:
        num //= base
        e += 1

    return e
