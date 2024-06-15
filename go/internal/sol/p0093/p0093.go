package p0093

import (
	"math/big"
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func makeNumbers(lst []*big.Rat) map[int]struct{} {
	nSet := map[int]struct{}{}
	ratZero := big.NewRat(0, 1)

	var aux func([]*big.Rat)
	aux = func(lst []*big.Rat) {
		if len(lst) == 1 {
			if lst[0].IsInt() {
				nSet[int(lst[0].Num().Int64())] = struct{}{}
			}
			return
		}

		for i, d1 := range lst {
			for j, d2 := range lst[i+1:] {
				nextLst := slices.Delete(slices.Clone(lst), (i+1)+j, (i+1)+j+1)
				nextLst = slices.Delete(nextLst, i, i+1)
				aux(slices.Concat([]*big.Rat{new(big.Rat).Add(d1, d2)}, nextLst))
				aux(slices.Concat([]*big.Rat{new(big.Rat).Mul(d1, d2)}, nextLst))
				aux(slices.Concat([]*big.Rat{new(big.Rat).Sub(d1, d2)}, nextLst))
				aux(slices.Concat([]*big.Rat{new(big.Rat).Sub(d2, d1)}, nextLst))
				if d1.Cmp(ratZero) != 0 {
					aux(slices.Concat([]*big.Rat{new(big.Rat).Mul(d2, new(big.Rat).Inv(d1))}, nextLst))
				}
				if d2.Cmp(ratZero) != 0 {
					aux(slices.Concat([]*big.Rat{new(big.Rat).Mul(d1, new(big.Rat).Inv(d2))}, nextLst))
				}
			}
		}
	}

	aux(lst)

	return nSet
}

func countConsecNumbers(lst []int) int {
	ratLst := make([]*big.Rat, 0)
	for _, x := range lst {
		ratLst = append(ratLst, big.NewRat(int64(x), 1))
	}

	nSet := makeNumbers(ratLst)
	for cnt := 1; ; cnt++ {
		if _, ok := nSet[cnt]; ok {
			continue
		}
		return cnt - 1
	}
}

func compute() string {
	var maxCount int
	var nums []int

	ch := mylib.Combinations([]int{1, 2, 3, 4, 5, 6, 7, 8, 9}, 4)
	for lst := range ch {
		tmp := countConsecNumbers(lst)
		if tmp > maxCount {
			maxCount = tmp
			nums = lst
		}
	}

	var result int
	for _, x := range nums {
		result = result*10 + x
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute()
}
