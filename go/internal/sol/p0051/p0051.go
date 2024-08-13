package p0051

/*
1) it is clear that the last digit is not eligible for replacement

2) the number of digits that can be replaced is only multiples of 3
   if 'n' is not multiples of 3, some replaced numbers will contain multiples of 3.

     number  digits  sum  'mod 3'    [n>0]
     ---------------------------------------------
     0       n       0    0
     1       n       n    n mod 3
     2       n       2n   2n mod 3
     3       n       3n   3n mod 3 = 0
     4       n       4n   4n mod 3 = n mod 3
     5       n       5n   5n mod 3 = 2n mod 3
     6       n       6n   6n mod 3 = 0
     7       n       7n   7n mod 3 = n mod 3
     8       n       8n   8n mod 3 = 2n mod 3
     9       n       9n   9n mod 3 = 0

3) because of 1) and 2), prime numbers are larger than 1000.
*/

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func isFamily(p, familySize int) bool {
	pDigits := mylib.Digits(p)
	for n := 0; n <= 10-familySize; n++ {
		for positions := range mylib.PowerSet(mylib.FindAll(pDigits, func(x int) bool { return x == n })) {
			if len(positions) < 3 || len(positions)%3 != 0 || positions[0] == 0 {
				continue
			}
			cnt := 1
			work := slices.Clone(pDigits)
			for d := n + 1; d <= 9; d++ {
				for pos := range slices.Values(positions) {
					work[pos] = d
				}
				if mylib.IsPrime(mylib.UnDigits(work)) {
					cnt++
				}

				if cnt == familySize {
					return true
				} else if (familySize - cnt) > (9 - d) {
					break
				}
			}
		}
	}

	return false
}

func compute(familySize int) string {
	for p := mylib.NextPrime(1000); ; p = mylib.NextPrime(p) {
		if isFamily(p, familySize) {
			return strconv.FormatInt(int64(p), 10)
		}
	}
}

func Solve() string {
	return compute(8)
}
