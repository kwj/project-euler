package p0052

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func checkNumber(n int) bool {
	getKey := func(n int) string {
		tmp := []rune(strconv.FormatInt(int64(n), 10))
		slices.Sort(tmp)

		return string(tmp)
	}

	id := getKey(n)
	for mult := 2; mult <= 6; mult++ {
		if id != getKey(n*mult) {
			return false
		}
	}

	return true
}

func compute() string {
	for e := 6; ; e++ {
		for n := mylib.Pow(10, e-1); n <= mylib.Pow(10, e)/6+1; n++ {
			if checkNumber(n) {
				return strconv.FormatInt(int64(n), 10)
			}
		}
	}
}

func Solve() string {
	return compute()
}
