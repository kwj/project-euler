package p0053

import (
	"strconv"
)

func compute(num, boundary int) string {
	if num < 1 {
		panic("range error")
	}

	n := num
	x := num
	c := 1
	r := 1
	var result int
	if boundary == 0 {
		result = num * 2
	}

	for r <= n/2 {
		if c = c * x / r; c > boundary {
			result += n - (r * 2) + 1
			c = c * r / n
			n--
		} else {
			r++
		}
		x--
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(100, 1_000_000)
}
