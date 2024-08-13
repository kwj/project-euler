package p0043

import (
	"slices"
	"strconv"
	"strings"
)

func compute() string {
	lst := []string{""}
	for d := range slices.Values([]int{1, 1, 17, 13, 11, 7, 5, 3, 2, 1}) {
		nextLst := []string{}
		for s := range slices.Values(lst) {
			for x := range slices.Values([]rune{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}) {
				if !strings.ContainsRune(s, x) {
					nextStr := string(x) + s
					if len(nextStr) < 3 {
						nextLst = append(nextLst, nextStr)
					} else {
						if n, _ := strconv.Atoi(nextStr[:3]); n%d == 0 {
							nextLst = append(nextLst, nextStr)
						}
					}
				}
			}
		}
		lst = nextLst
	}

	var result int
	for s := range slices.Values(lst) {
		n, _ := strconv.Atoi(s)
		result += n
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute()
}
