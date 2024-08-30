package p0038

/*
It is clear that number X is within 4 digits from the problem statement.

  1) if number X is four digits, n = 2
      (X * 1 -> 4-digits, X * 2 -> 5-digits)
  2) if number X is three digits, n = 3
      (X * 1 -> 3-digits, X * 2 -> 3-digits, X * 3 -> 3-digits)
  3) if number X is two digits, n = 4
      (X * 1 -> 2-digits, X * 2 -> 2-digits, X * 3 -> 2-digits, X * 4 -> 3-digits)
  4) if number X is one digit, n = 9 or 5
      (only X=1 and n=9, X=9 and n=5).

case #1:
  5000 <= X <= 9999
case #2:
  100 <= X <= 333
case #3:
  25 <= X <= 33
case #4:
  X = 1, 9

Since we already know the concatenated product of 9 and (1,2,3,4,5) is 918273645,
actually, the range of candidates is [9183, 9999].

Furthermore, multiplicand `9abc` times 2 must be `18xyz` not `19xyz`.
It is clear that the range of candidates is only [9183, 9499].
Finally, the last digit of numbers is 0, 1, 4, 5, 8 or 9 can be excluded from candidates.
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute() string {
	ans := 918273645
	for i := 9499; i >= 9183; i-- {
		rem := i % 10
		if rem <= 1 || rem >= 8 || rem == 4 || rem == 5 {
			continue
		}

		if x := i * 100_002; mylib.IsPandigitalNZ(x) {
			ans = x
			break
		}
	}

	return strconv.FormatInt(int64(ans), 10)
}

func Solve() string {
	return compute()
}
