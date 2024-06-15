package p0063

/*
  n - 1 <= log10(m^n) < n    [m>0, n>0]
    --> n - 1 <= n * log10(m) < n
    --> m < 10
  and...
    --> (n - 1)/n <= log10(m)
    --> n/n - (n -1)/n >= 1 - log10(m)
    --> 1/n >= 1 - log10(m)
    --> 1/(1 - log10(m)) >= n
    --> max(n) = floor(1/(1 - log10(m)))
*/

import (
	"math"
	"strconv"
)

func compute() string {
	var result int
	for m := 1; m <= 9; m++ {
		result += int(math.Floor(1 / (1 - math.Log10(float64(m)))))
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute()
}
