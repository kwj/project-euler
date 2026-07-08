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
	if ndigits == 1 {
		nLower = 0
	}
	var blkWidth = mylib.Pow(10, (ndigits*2 - 2))
	var answer = make([]int, 0, 1)

	for blkLower := ((nUpper * nUpper) / blkWidth) * blkWidth; blkLower >= (nLower * nLower); blkLower -= blkWidth {
		var blkUpper = blkLower + blkWidth - 1
		for x := nUpper; x >= nLower; x-- {
			if x*x < blkLower {
				break
			}
			var yStart = x
			if x > 0 {
				yStart = min(blkUpper/x, x)
			}
			for y := yStart; y >= nLower; y-- {
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
