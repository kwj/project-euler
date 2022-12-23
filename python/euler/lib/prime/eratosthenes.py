# -*- coding: utf-8 -*-

"""Sieve of Eratosthenes

  reminders divided by 30
  [1, 7, 11, 13, 17, 19, 23, 29]

  prime: p = 30q + r

    p * p : start (cmposite)
    p * (p + D) : p + D is other than mulitles of 2, 3 or 5
      ...
     example: p = 7
       7 * 7, 7 * 11, 7 * 13, 7 * 17, 7 * 19, 7 * 23, 7 * 29, 7 * 31, ...

  prime table:
    byte 0: 1-30
    byte 1: 31-60
      ...

      byte q: MSB    LSB
               xxxxxxxx   // r_bit,b_bit: 0(LSB) .. 7(MSB)
               ||||||||
               |||||||+-- 30q + 1
               ||||||+--- 30q + 7
               |||||+---- 30q + 11
               ||||+----- 30q + 13
               |||+------ 30q + 17
               ||+------- 30q + 19
               |+-------- 30q + 23
               +--------- 30q + 29

  // start : p * p = p^2
  p^2 = (30q + r) * (30q + r)
      = 900q^2 + 60qr + r^2

    // byte position (index for prime table)
    p^2/30 = 30q^2 + 2qr + r^2/30
           = q(30q + 2r) + r^2/30
             ^^^^^^^^^^^^^^^^^^^^ [*1]
    // bit position information
    p^2 % 30 = r^2 % 30
               ^^^^^^^^ [*2]
  // D: diff
  p * k{i} = (30q + r) * (30a + b)
  p * k{i+1} = (30q + r) * (30a + b + D)
             = (30q + r) * (30a + c)

    // offset of byte position
    k{i+1}*p/30 - k{i}*p/30 = ((30a + c) * (30q + r))/30 - ((30a + b) * (30q + r))/30
                            = (900aq + 30ar + 30cq + cr)/30 - (900aq + 30ar + 30bq + br)/30
                            = q(c-b) + cr/30 - br/30
                                       ^^^^^^^^^^^^^ [*3]
                              ^^^^^^^^^^^^^^^^^^^^^^ [*4]
    // bit position information
    (k{i+1} * p) % 30 = cr % 30
                        ^^^^^^^ [*2]

  --------
  // [*2] mask data to clear the corresponding bit
    __mask_tbl = [[0xFE, 0xFD, 0xFB, 0xF7, 0xEF, 0xDF, 0xBF, 0x7F],
                  [0xFD, 0xDF, 0xEF, 0xFE, 0x7F, 0xF7, 0xFB, 0xBF],
                  [0xFB, 0xEF, 0xFE, 0xBF, 0xFD, 0x7F, 0xF7, 0xDF],
                  [0xF7, 0xFE, 0xBF, 0xDF, 0xFB, 0xFD, 0x7F, 0xEF],
                  [0xEF, 0x7F, 0xFD, 0xFB, 0xDF, 0xBF, 0xFE, 0xF7],
                  [0xDF, 0xF7, 0x7F, 0xFD, 0xBF, 0xFE, 0xEF, 0xFB],
                  [0xBF, 0xFB, 0xF7, 0x7F, 0xFE, 0xEF, 0xDF, 0xFD],
                  [0x7F, 0xBF, 0xDF, 0xEF, 0xF7, 0xFB, 0xFD, 0xFE]]

    rem = [1,7,11,13,17,19,23,29]
    m = {1:0xFE, 7:0xFD, 11:0xFB, 13:0xF7, 17:0xEF, 19:0xDF, 23:0xBF, 29:0x7F}
    >>> [[hex(m[(x * y) % 30]) for y in rem] for x in rem]
    [['0xfe', '0xfd', '0xfb', '0xf7', '0xef', '0xdf', '0xbf', '0x7f'],
     ['0xfd', '0xdf', '0xef', '0xfe', '0x7f', '0xf7', '0xfb', '0xbf'],
     ['0xfb', '0xef', '0xfe', '0xbf', '0xfd', '0x7f', '0xf7', '0xdf'],
     ['0xf7', '0xfe', '0xbf', '0xdf', '0xfb', '0xfd', '0x7f', '0xef'],
     ['0xef', '0x7f', '0xfd', '0xfb', '0xdf', '0xbf', '0xfe', '0xf7'],
     ['0xdf', '0xf7', '0x7f', '0xfd', '0xbf', '0xfe', '0xef', '0xfb'],
     ['0xbf', '0xfb', '0xf7', '0x7f', '0xfe', '0xef', '0xdf', '0xfd'],
     ['0x7f', '0xbf', '0xdf', '0xef', '0xf7', '0xfb', '0xfd', '0xfe']]


  // [*3] cr/30 - br/30 : rb_tbl[r][b]
    rb_tbl = [[0, 0, 0, 0, 0, 0, 0, 1], [1, 1, 1, 0, 1, 1, 1, 1],
              [2, 2, 0, 2, 0, 2, 2, 1], [3, 1, 1, 2, 1, 1, 3, 1],
              [3, 3, 1, 2, 1, 3, 3, 1], [4, 2, 2, 2, 2, 2, 4, 1],
              [5, 3, 1, 4, 1, 3, 5, 1], [6, 4, 2, 4, 2, 4, 6, 1]]

    rem = [1,7,11,13,17,19,23,29]
    c = {1:7, 7:11, 11:13, 13:17, 17:19, 19:23, 23:29, 29:31}
    >>> [[(c[b] * r) // 30 - (b * r) // 30 for b in rem] for r in rem]
    [[0, 0, 0, 0, 0, 0, 0, 1], [1, 1, 1, 0, 1, 1, 1, 1],
     [2, 2, 0, 2, 0, 2, 2, 1], [3, 1, 1, 2, 1, 1, 3, 1],
     [3, 3, 1, 2, 1, 3, 3, 1], [4, 2, 2, 2, 2, 2, 4, 1],
     [5, 3, 1, 4, 1, 3, 5, 1], [6, 4, 2, 4, 2, 4, 6, 1]]

  // [*4] c-b: difference of elements of mod30
    mod30_diff = [6, 4, 2, 4, 2, 4, 6, 2]

    c -> 7; 11; 13; 17; 19; 23; 29; 31
    b -> 1;  7; 11; 13; 17; 19; 23; 29
    -----------------------------------
         6;  4;  2;  4;  2;  4;  6;  2


"""

import array
from math import isqrt
from functools import reduce

class Eratosthenes:
    __mod30 = [1, 7, 11, 13, 17, 19, 23, 29]

    # [*2]
    __mask_tbl = [[0xFE, 0xFD, 0xFB, 0xF7, 0xEF, 0xDF, 0xBF, 0x7F],
                  [0xFD, 0xDF, 0xEF, 0xFE, 0x7F, 0xF7, 0xFB, 0xBF],
                  [0xFB, 0xEF, 0xFE, 0xBF, 0xFD, 0x7F, 0xF7, 0xDF],
                  [0xF7, 0xFE, 0xBF, 0xDF, 0xFB, 0xFD, 0x7F, 0xEF],
                  [0xEF, 0x7F, 0xFD, 0xFB, 0xDF, 0xBF, 0xFE, 0xF7],
                  [0xDF, 0xF7, 0x7F, 0xFD, 0xBF, 0xFE, 0xEF, 0xFB],
                  [0xBF, 0xFB, 0xF7, 0x7F, 0xFE, 0xEF, 0xDF, 0xFD],
                  [0x7F, 0xBF, 0xDF, 0xEF, 0xF7, 0xFB, 0xFD, 0xFE]]

    # [*3], [*4]
    def __get_idx_offset(self, q, r, b):
        mod30_diff = [6, 4, 2, 4, 2, 4, 6, 2]
        rb_tbl = [[0, 0, 0, 0, 0, 0, 0, 1], [1, 1, 1, 0, 1, 1, 1, 1],
                  [2, 2, 0, 2, 0, 2, 2, 1], [3, 1, 1, 2, 1, 1, 3, 1],
                  [3, 3, 1, 2, 1, 3, 3, 1], [4, 2, 2, 2, 2, 2, 4, 1],
                  [5, 3, 1, 4, 1, 3, 5, 1], [6, 4, 2, 4, 2, 4, 6, 1]]

        return q * mod30_diff[b] + rb_tbl[r][b]

    def __get_term_elt(self, n):
        match n % 30:
            case 0: return 0xFF
            case x if x >= 29: return 0xFF
            case x if x >= 23: return 0x7F
            case x if x >= 19: return 0x3F
            case x if x >= 17: return 0x1F
            case x if x >= 13: return 0x0F
            case x if x >= 11: return 0x07
            case x if x >= 7: return 0x03
            case _: return 0x01

    def __get_bit_pos(self, n):
        match n:
            case 1: return 0
            case 2: return 1
            case 4: return 2
            case 8: return 3
            case 16: return 4
            case 32: return 5
            case 64: return 6
            case 128: return 7
            case _: raise ValueError("bad argument")

    def __get_msb(self, n):
        n &= 0xFF
        n |= (n >> 1)
        n |= (n >> 2)
        n |= (n >> 4)
        return n ^ (n >> 1)

    def __get_lsb(self, n):
        return n & (-n)

    def __init__(self, num):
        if num < 2:
            raise ValueError("argument too small")
        self.size = num
        self.tbl_size = (num + 29) // 30
        self.prime_tbl = array.array('B')
        self.prime_tbl.extend((0xFF,) * self.tbl_size)
        self.prime_tbl[self.tbl_size - 1] = self.__get_term_elt(num)
        self.prime_tbl[0] &= 0xFE    # '1' isn't a composite number

        for q, flag in enumerate(self.prime_tbl):
            while flag != 0:
                r_bit = b_bit = self.__get_bit_pos(self.__get_lsb(flag))
                r = self.__mod30[r_bit]
                idx = q * (30 * q + 2 * r) + (r * r) // 30    # [*1]
                while idx < self.tbl_size:
                    # turn off the bit since it's a composite number
                    self.prime_tbl[idx] &= self.__mask_tbl[r_bit][b_bit]

                    idx += self.__get_idx_offset(q, r_bit, b_bit)
                    b_bit = (b_bit + 1) & 0b00000111
                flag &= (flag - 1)    # turn off the rightmost 1-bit

    def is_prime(self, num):
        if num > self.size:
            raise ValueError("argument too large")
        match num:
            case 2 | 3 | 5:
                return True
            case _:
                if num % 2 == 0 or num % 3 == 0 or num % 5 == 0:
                    return False
                else:
                    return self.prime_tbl[num // 30] & self.__get_msb(self.__get_term_elt(num)) != 0

    def count(self):
        match self.size:
            case 2:
                return 1
            case n if n < 5:
                return 2
            case n if n < 7:
                return 3
            case _:
                return reduce(lambda acc, x: acc + x.bit_count(), self.prime_tbl, 3)

    def to_list(self):
        def aux(tpl, flag):
            q = tpl[0]
            lst = tpl[1]
            while flag != 0:
                lst.append(30 * q + self.__mod30[self.__get_bit_pos(flag & (-flag))])
                flag &= (flag - 1)
            return (q + 1, lst)

        match self.size:
            case 2:
                return [2]
            case n if n < 5:
                return [2, 3]
            case n if n < 7:
                return [2, 3, 5]
            case _:
                return reduce(lambda acc_tpl, x: aux(acc_tpl, x), self.prime_tbl, (0, [2, 3, 5]))[1]

    def prev_prime(self, num):
        match num:
            case n if n > self.size:
                raise ValueError("too large")
            case n if n <= 2:
                raise ValueError("too small")
            case n if n == 3:
                return 2
            case n if n <= 5:
                return 3
            case n if n <= 7:
                return 5
            case n:
                def aux(idx, flags):
                    if idx < 0:
                        raise ValueError("not found")
                    elif self.prime_tbl[idx] & flags == 0:
                        return aux(idx - 1, 0xFF)
                    else:
                        return 30 * idx + self.__mod30[self.__get_bit_pos(self.__get_msb(self.prime_tbl[idx] & flags))]

                if n % 30 != 1:
                    return aux((n - 1) // 30, self.__get_term_elt(n - 1))
                else:
                    return aux((n - 2) // 30, 0xFF)

    def next_prime(self, num):
        def get_max_prime(n):
            idx = self.tbl_size - 1
            while self.prime_tbl[idx] == 0:
                idx -= 1

            return 30 * idx + self.__mod30[self.__get_bit_pos(self.__get_msb(self.prime_tbl[idx]))]

        match num:
            case n if n >= get_max_prime(n):
                raise ValueError("too large")
            case n if n < 0:
                raise ValueError("too small")
            case n if n < 2:
                return 2
            case n if n < 3:
                return 3
            case n if n < 5:
                return 5
            case n:
                def aux(idx, flags):
                    if idx >= self.tbl_size:
                        raise ValueError("not found")
                    elif self.prime_tbl[idx] & flags == 0:
                        return aux(idx + 1, 0xFF)
                    else:
                        return 30 * idx + self.__mod30[self.__get_bit_pos(self.__get_lsb(self.prime_tbl[idx] & flags))]

                if n % 30 == 0:
                    return aux(n // 30, 0xFF)
                else:
                    return aux(n // 30, ~(self.__get_term_elt(n)) & 0xFF)

def eratosthenes(num):
    return Eratosthenes(num)
