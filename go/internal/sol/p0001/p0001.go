package p0001

import "strconv"

func sumMultiples(n, limit uint) uint {
	limit--

	return (n + (limit - (limit % n))) * (limit / n) / 2
}

func compute(limit uint) string {
	return strconv.FormatUint(
		uint64(sumMultiples(3, limit)+sumMultiples(5, limit)-sumMultiples(15, limit)),
		10,
	)
}

func Solve() string {
	return compute(1_000)
}
