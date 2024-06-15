package mylib

import (
	"math/big"
)

func NumOfDigits(n, base int) int {
	return MaxExp(n, base) + 1
}

func IsPalindrome(n, base int) bool {
	x := n
	acc := 0
	for x > 0 {
		acc = acc*base + (x % base)
		x /= base
	}

	return acc == n
}

func IsPandigital(n int) bool {
	mkBits := func(i int) int {
		bits := 0
		for i > 0 {
			bits |= (1 << (i % 10))
			i /= 10
		}

		return bits
	}

	return mkBits(n) == (1<<NumOfDigits(n, 10))-1
}

func IsPandigitalNZ(n int) bool {
	checkZero := func(i int) bool {
		for i > 0 {
			if i%10 == 0 {
				return false
			}
			i /= 10
		}

		return true
	}

	return checkZero(n) && IsPandigital(n*10)
}

func Digits(n int) []int {
	result := make([]int, 0, NumOfDigits(n, 10))
	for n > 0 {
		result = append(result, n%10)
		n /= 10
	}

	return result
}

func UnDigits(ns []int) int {
	result := 0
	for idx := len(ns) - 1; idx >= 0; idx-- {
		result = result*10 + ns[idx]
	}

	return result
}

func DigitsBig(n *big.Int) []int {
	result := make([]int, 0)
	tmp := big.NewInt(0)
	base := big.NewInt(10)

	for n.Cmp(big.NewInt(0)) != 0 {
		tmp.Mod(n, base)
		result = append(result, int(tmp.Int64()))
		n.Div(n, base)
	}

	return result
}

func UndigitsBig(ns []int) *big.Int {
	base := big.NewInt(10)
	result := big.NewInt(0)

	for idx := len(ns) - 1; idx >= 0; idx-- {
		result.Mul(result, base)
		result.Add(result, big.NewInt(int64(ns[idx])))
	}

	return result
}
