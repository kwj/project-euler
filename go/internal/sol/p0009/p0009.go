package p0009

/*
a = k(m^2 - n^2), b = k * 2mn, c = k(m^2 + n^2)  [m>n>0, gcd(m,n)=1, m+n is odd]

abc = k^3 * (m^4 - n^4) * 2mn
a + b + c = k * 2m(m+n) = 1000

 -> 'k' and 'm' are divisors to 500 (= 1000/2).
    'm+n' is a divisor to 500/m.
    m(m+n) <= 500 --> m <= isqrt(500), m+n <= 500/m
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(perim int) string {
	perim /= 2
	for m := 2; m <= int(mylib.Isqrt(perim)); m++ {
		if perim%m != 0 {
			continue
		}

		// x = m + n ('x' is odd number)
		x := m + 1 + (m % 2)
		for x < 2*m && x <= perim/m {
			if mylib.Gcd(m, x) == 1 && (perim/m)%x == 0 {
				// According to the problem statement, there is only one answer.
				k := perim / m / x
				n := x - m
				return strconv.FormatInt(
					int64(mylib.Pow(k, 3)*(mylib.Pow(m, 4)-mylib.Pow(n, 4))*2*m*n),
					10,
				)
			}
			x += 2
		}

	}

	panic("unreachable")
}

func Solve() string {
	return compute(1_000)
}
