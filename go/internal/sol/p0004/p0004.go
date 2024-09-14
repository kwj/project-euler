package p0004

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(ndigits int) string {
	if ndigits <= 0 {
		panic("argument must be a positive number")
	}
	var nUpper = mylib.Pow(10, ndigits) - 1
	var nLower = mylib.Pow(10, ndigits-1)
	var blkUpperLimit = mylib.Pow(10, ndigits*2)
	var blkLowerLimit = 0
	if ndigits > 1 {
		blkLowerLimit = mylib.Pow(10, (ndigits-1)*2)
	}
	var blkWidth = mylib.Pow(10, (ndigits*2 - 2))
	var answer = make([]int, 0, 1)

	for blkLower := blkUpperLimit - blkWidth; blkLower >= blkLowerLimit; blkLower -= blkWidth {
		var blkUpper = blkLower + blkWidth - 1
		for x := nUpper; x >= nLower; x-- {
			if x*x < blkLower {
				break
			}
			for y := min(blkUpper/x, x); y >= nLower; y-- {
				var tmp = x * y
				if tmp < blkLower {
					break
				}
				if mylib.IsPalindrome(tmp, 10) {
					answer = append(answer, tmp)
				}
			}
			if len(answer) != 0 {
				return strconv.FormatInt(int64(slices.Max(answer)), 10)
			}
		}
	}

	panic("unreachable")
}

func Solve() string {
	return compute(3)
}
