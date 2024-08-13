package p0037

/*
candidate numbers: [2357][1379]*[37] (n >= 10)
*/

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func addPrefixNumber(preLst, lst []int) []int {
	result := make([]int, 0)
	for p := range slices.Values(preLst) {
		for n := range slices.Values(lst) {
			result = append(result, p*(mylib.Pow(10, mylib.NumOfDigits(n, 10)))+n)
		}
	}

	return result
}

func makeNextList(lst []int) []int {
	result := make([]int, 0)
	for n := range slices.Values(addPrefixNumber([]int{1, 3, 7, 9}, lst)) {
		if mylib.IsPrime(n) {
			result = append(result, n)
		}
	}

	return result
}

func pickupPrimes(lst []int) []int {
	isTruncablePrime := func(n int) bool {
		if n == 0 {
			return false
		}
		for n != 0 {
			if !mylib.IsPrime(n) {
				return false
			}
			n /= 10
		}

		return true
	}

	result := make([]int, 0)
	for n := range slices.Values(addPrefixNumber([]int{2, 3, 5, 7}, lst)) {
		if isTruncablePrime(n) {
			result = append(result, n)
		}
	}

	return result
}

func compute() string {
	result := make([]int, 0)
	lst := []int{3, 7}

	for len(result) < 11 {
		result = slices.Concat(result, pickupPrimes(lst))
		lst = makeNextList(lst)
	}
	if len(result) != 11 {
		panic("too many candidates")
	}

	var acc int
	for v := range slices.Values(result) {
		acc += v
	}

	return strconv.FormatInt(int64(acc), 10)
}

func Solve() string {
	return compute()
}
