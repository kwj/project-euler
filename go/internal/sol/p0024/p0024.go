package p0024

import (
	"fmt"
	"slices"
	"strings"

	"pe-solver/internal/mylib"
)

func factorialTbl(n, depth int) []int {
	result := make([]int, 0)
	i := n - 1
	divisor := mylib.Factorial(n - depth)
	for range depth {
		result = append(result, mylib.Factorial(i)/divisor)
		i--
	}
	return result
}

func compute(nth int, elmLst []int, depth int) string {
	idx := nth - 1
	result := make([]string, 0)
	for _, n := range factorialTbl(len(elmLst), depth) {
		i := idx / n
		result = append(result, fmt.Sprint(elmLst[i]))
		elmLst = slices.Delete(elmLst, i, i+1)
		idx %= n
	}

	return strings.Join(result, "")
}

func Solve() string {
	lst := []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
	return compute(1_000_000, lst, len(lst))
}
