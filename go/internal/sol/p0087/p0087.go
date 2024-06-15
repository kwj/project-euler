package p0087

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(limit int) string {
	type intSet map[int]struct{}
	nSet := intSet{}
	sqLst := mylib.Primes(mylib.Isqrt(limit - mylib.Pow(2, 3) - mylib.Pow(2, 4)))
	cbLst := slices.DeleteFunc(slices.Clone(sqLst), func(x int) bool { return mylib.Pow(x, 3) > limit })
	quLst := slices.DeleteFunc(slices.Clone(cbLst), func(x int) bool { return mylib.Pow(x, 4) > limit })

	for _, z := range quLst {
		for _, y := range cbLst {
			for _, x := range sqLst {
				if tmp := mylib.Pow(x, 2) + mylib.Pow(y, 3) + mylib.Pow(z, 4); tmp < limit {
					nSet[tmp] = struct{}{}
				}
			}
		}
	}

	return strconv.FormatInt(int64(len(nSet)), 10)
}

func Solve() string {
	return compute(50_000_000)
}
