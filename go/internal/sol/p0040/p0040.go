package p0040

/*
  0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
    ---------   -----------------   ---------------------   -----------------
len: 1 * 9       2 * 90              3 * 900                 4 * 9000      ...
     1 * 9 * 1   2 * 9 * 10          3 * 9 * 100             4 * 9 * 1000  ...
       --> block_num * 9 * base

block #1: 1-digit number
block #2: 2-digits number
block #3: 3-digits number
  ...
block #n: n-digits number
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

type pair struct {
	ndigit int
	maxNth int
}

func getBlock(nth int) pair {
	var n = 1
	var maxNth = 9
	for maxNth < nth {
		n, maxNth = n+1, maxNth+(n+1)*9*mylib.Pow(10, n)
	}

	return pair{n, maxNth}
}

func d(nth int) int {
	blk := getBlock(nth)
	n := mylib.Pow(10, blk.ndigit) - 1 - (blk.maxNth-nth)/blk.ndigit
	idx := (blk.maxNth - nth) % blk.ndigit

	return mylib.Digits(n)[idx]
}

func compute() string {
	indices := []int{1, 10, 100, 1_000, 10_000, 100_000, 1_000_000}
	var result = 1
	for _, nth := range indices {
		result *= d(nth)
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute()
}
