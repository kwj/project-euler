package p0093

/*
combination of numberes:
  nCk = C(n,k) = C(9,4) = 9*8*7*6 / 4*3*2*1 = 126

arithmetic operations (fourOps):
  commutative:
    addition: X + Y
    multiplication: X * Y
  no-commutative:
    subtraction:  X - Y, Y - X
    division: X / Y, Y / X

patterns:
  A, B, C, D: numbers
    4! = 24

  [1] ((A op B) op C) op D
       ^^^^^^^^
  [2] (A op B) op (C op D)
      ^^^^^^^^    ^^^^^^^^
	  ^^^^^^^^^^^^^^^^^^^^

  ^^^: The order of the two terms is irrelevant
       because fourOps() also considers the reverse order.
*/

import (
	"math/big"
	"strconv"

	"pe-solver/internal/mylib"
)

var ratZero = big.NewRat(0, 1)

func fourOps(x1, x2 *big.Rat) []*big.Rat {
	result := make([]*big.Rat, 0)

	result = append(result, new(big.Rat).Add(x1, x2))
	result = append(result, new(big.Rat).Mul(x1, x2))
	result = append(result, new(big.Rat).Sub(x1, x2))
	result = append(result, new(big.Rat).Sub(x2, x1))
	if x1.Cmp(ratZero) != 0 {
		result = append(result, new(big.Rat).Mul(x2, new(big.Rat).Inv(x1)))
	}
	if x2.Cmp(ratZero) != 0 {
		result = append(result, new(big.Rat).Mul(x1, new(big.Rat).Inv(x2)))
	}

	return result
}

func case1(d1, d2 *big.Rat, rest []*big.Rat) []*big.Rat {
	result := make([]*big.Rat, 0)
	for _, ab := range fourOps(d1, d2) {
		// c: rest[0], d: rest[1]
		for _, abc := range fourOps(ab, rest[0]) {
			result = append(result, fourOps(abc, rest[1])...)
		}
		// c: rest[1], d: rest[0]
		for _, abc := range fourOps(ab, rest[1]) {
			result = append(result, fourOps(abc, rest[0])...)
		}
	}

	return result
}

func case2(d1, d2 *big.Rat, rest []*big.Rat) []*big.Rat {
	result := make([]*big.Rat, 0)
	ab := fourOps(d1, d2)
	cd := fourOps(rest[0], rest[1])

	ch := mylib.CartesianProduct(ab, cd)
	for pair := range ch {
		result = append(result, fourOps(pair[0], pair[1])...)
	}

	return result
}

func makeNumbers(lst []*big.Rat) map[int]struct{} {
	nSet := map[int]struct{}{}

	for i := 0; i < len(lst); i++ {
		for j := i + 1; j < len(lst); j++ {
			rest := make([]*big.Rat, 0, 2)
			for k := 0; k < len(lst); k++ {
				if k != i && k != j {
					rest = append(rest, lst[k])
				}
			}

			for _, x := range case1(lst[i], lst[j], rest) {
				if x.IsInt() {
					nSet[int(x.Num().Int64())] = struct{}{}
				}
			}
			for _, x := range case2(lst[i], lst[j], rest) {
				if x.IsInt() {
					nSet[int(x.Num().Int64())] = struct{}{}
				}
			}
		}
	}

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
