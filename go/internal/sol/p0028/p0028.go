package p0028

/*
21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13
       |  |  |  |
    (n=0, 1, 2, 3, ...)

the upper right number is:
  1    [n=0]
  (2n+1)**2    [n=>1]

so, the sum of numbers in the four corners is:
  (2n+1)**2 + ((2n+1)**2 - 2n) + ((2n+1)**2 - 4n) + ((2n+1)**2 - 6n)
    = 16n**2 + 4n + 4   [n>=1]

Answer: 1 + sum_{n=1}^{(1001-1)/2} (16n**2 + 4n + 4)
*/

import (
	"strconv"
)

func compute(length int) string {
	var result = 1
	for n := 1; n <= (length-1)/2; n++ {
		result += 16*n*n + 4*n + 4
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_001)
}
