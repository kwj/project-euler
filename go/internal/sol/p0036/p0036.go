package p0036

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(limit int) string {
	var result int
	for i := 1; i < limit; i += 2 {
		if mylib.IsPalindrome(i, 10) && mylib.IsPalindrome(i, 2) {
			result += i
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000_000)
}
