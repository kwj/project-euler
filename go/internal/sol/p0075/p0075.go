package p0075

/*
Pythagorean triple

  a = k * (m^2 - n^2), b = k * 2mn, c = k * (m^2 + n^2)
    where m > n > 0, gcd(m, n) = 1

  perimeter L = k * (2m^2 + 2mn)
              = k * 2m(m + n)

  2m(m + n) = L/k
    -->
  2m^2 < 2m(m + n) = L/k
    <-->
  m^2 < L/2k

  'm' is maximized when k=1
    max(m) < sqrt(L/2)
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(length int) string {
	limit := mylib.Isqrt(length / 2)
	counter := make([]int, length+1)

	for m := 2; m <= limit; m++ {
		for n := 1 + (m & 1); n < m; n += 2 {
			if mylib.Gcd(m, n) != 1 {
				continue
			}
			perimeter := 2 * m * (m + n)
			if perimeter > length {
				break
			}
			for idx := perimeter; idx <= length; idx += perimeter {
				counter[idx]++
			}
		}
	}

	var result int
	for _, v := range counter {
		if v == 1 {
			result++
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_500_000)
}
