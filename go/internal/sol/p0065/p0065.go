package p0065

/*
  e = [2; 1, 2, 1, 1, 4, 1, 1, 6, ..., 1, 1, 2k, ...]
      [a{0}; a{1}, a{2}, ...]

    i  a{i-1}  n(numerator)  d(denominator)
   ----------------------------------------
    1   2         2             1
    2   1         3             1
    3   2         8             3
    4   1        11             4
    5   1        19             7
    6   4        87            32
    7   1       106            39
    8   1       193            71
    9   6      1264           465
   10   1      1457           536
             ...
    i c(i)     n(i)          d(i)

    when i > 2:
      n(i) = n(i-1)*c(i) + n(i-2), n(2) = 3, n(1) = 2
      d(i) = d(i-1)*c(i) + d(i-2), d(2) = 1, d(1) = 1

      c(i) = 1    (i mod 3 <> 0)
             2i/3 (i mod 3 = 0
*/

import (
	"math/big"
	"pe-solver/internal/mylib"
	"slices"
	"strconv"
)

func compute(nth int) string {
	sum := func(xs []int) int {
		var result int
		for v := range slices.Values(xs) {
			result += v
		}

		return result
	}

	c := func(i int) *big.Int {
		if i%3 != 0 {
			return big.NewInt(1)
		} else {
			result := big.NewInt(int64(i))
			result.Mul(result, big.NewInt(2))
			result.Div(result, big.NewInt(3))

			return result
		}
	}

	n1, n2 := big.NewInt(3), big.NewInt(2)
	for i := 3; i <= nth; i++ {
		tmp := c(i)
		tmp.Mul(tmp, n1)
		tmp.Add(tmp, n2)
		n1, n2 = tmp, n1
	}

	return strconv.FormatInt(int64(sum(mylib.DigitsBig(n1))), 10)
}

func Solve() string {
	return compute(100)
}
