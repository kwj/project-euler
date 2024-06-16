package p0060

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

type intSet map[int]struct{}

func pairablePrimes(x int, ascPrimes []int, limit int) []int {
	upper_x := mylib.Pow(10, mylib.NumOfDigits(x, 10))
	upper_p := 10
	result := make([]int, 0)
	for _, p := range ascPrimes {
		if x+p >= limit {
			break
		}
		for p > upper_p {
			upper_p *= 10
		}
		if mylib.IsPrime(x*upper_p+p) && mylib.IsPrime(p*upper_x+x) {
			result = append(result, p)
		}
	}

	return result
}

func findCliques(ascPrimes []int, size int, tbl map[int]intSet) [][]int {
	isAllPair := func(p int, group []int) bool {
		for _, x := range group {
			if _, ok := tbl[x][p]; !ok {
				return false
			}
		}

		return true
	}

	descPrimes := slices.Clone(ascPrimes)
	slices.Reverse(descPrimes)
	result := make([][]int, 0)

	var aux func([]int, []int, int)
	aux = func(group, descPrimes []int, depth int) {
		if depth == 0 {
			result = append(result, group)
		} else {
			for idx, p := range descPrimes[:len(descPrimes)-depth+1] {
				if len(group) == 0 || isAllPair(p, group) {
					aux(append(group, p), descPrimes[idx+1:], depth-1)
				}
			}
		}
	}

	aux(make([]int, 0), descPrimes, size)

	return result
}

func compute(groupSize int) string {
	primeGroups := [][]int{{3}, {3}}
	tbl := map[int]intSet{}
	size := groupSize - 1
	result := int(^uint(0) >> 1) // maxInt

	// skip 2, 3 and 5, and start with 7
	for p := 7; p < result; p = mylib.NextPrime(p) {
		grpIdx := (p % 3) - 1
		nbrs := pairablePrimes(p, primeGroups[grpIdx], result)
		numSet := intSet{}
		for _, x := range nbrs {
			numSet[x] = struct{}{}
		}
		tbl[p] = numSet
		primeGroups[grpIdx] = append(primeGroups[grpIdx], p)

		if len(nbrs) < size {
			continue
		}

		cliques := findCliques(nbrs, size, tbl)
		if len(cliques) > 0 {
			for _, clq := range cliques {
				acc := p
				for _, x := range clq {
					acc += x
				}
				result = min(result, acc)
			}
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(5)
}
