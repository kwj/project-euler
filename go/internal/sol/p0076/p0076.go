package p0076

/*
another version of problem 31

numbers: 1, 2, 3, ..., 99
total: 100
*/

import "strconv"

func compute(numbers []int, target int) string {
	tbl := make([]int, target+1)
	tbl[0] = 1

	for _, c := range numbers {
		for i := c; i <= target; i++ {
			tbl[i] += tbl[i-c]
		}
	}

	return strconv.FormatInt(int64(tbl[len(tbl)-1]), 10)
}

func Solve() string {
	const total = 100
	numbers := make([]int, total-1)
	for i := range numbers {
		numbers[i] = i + 1
	}

	return compute(numbers, total)
}
