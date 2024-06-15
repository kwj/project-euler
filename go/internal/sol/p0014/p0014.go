package p0014

import (
	"strconv"
)

func getCollatzLength(n int) int {
	var cnt = 1
	for n > 1 {
		cnt++
		if n&1 == 0 {
			n /= 2
		} else {
			n = 3*n + 1
		}
	}

	return cnt
}

func compute(limit int) string {
	var cnt int
	var maxCnt int
	var answer int
	for i := limit / 2; i < limit; i++ {
		if cnt = getCollatzLength(i); cnt > maxCnt {
			maxCnt = cnt
			answer = i
		}
	}

	return strconv.FormatInt(int64(answer), 10)
}

func Solve() string {
	return compute(1_000_000)
}
