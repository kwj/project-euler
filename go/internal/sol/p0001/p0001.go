package p0001

import "strconv"

func compute(limit uint) string {
	var f = func(x uint) uint {
		var tmp = (limit - 1) / x
		return (1 + tmp) * tmp / 2 * x
	}

	return strconv.FormatUint(uint64(f(3)+f(5)-f(15)), 10)
}

func Solve() string {
	return compute(1_000)
}
