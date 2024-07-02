package p0055

import (
	"math/big"
	"slices"
	"strconv"
)

func reverseBigNum(bignum *big.Int) *big.Int {
	result := new(big.Int)

	digits := []byte(bignum.String())
	slices.Reverse(digits)
	if _, ok := result.SetString(string(digits), 10); ok {
		return result
	}

	panic("unreachable")
}

func isLychrel(bignum *big.Int) bool {
	tmp := reverseBigNum(bignum)
	for range 50 {
		bignum.Add(bignum, tmp)
		tmp = reverseBigNum(bignum)
		if bignum.Cmp(tmp) == 0 {
			return false
		}
	}

	return true
}

func compute(limit int) string {
	var result int
	for i := 1; i <= limit; i++ {
		if isLychrel(big.NewInt(int64(i))) {
			result++
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(10_000)
}
