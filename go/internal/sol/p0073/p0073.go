package p0073

/*
f(n): number of fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2
      --> sigma{i=1, ...,n}((i-1)//2 - i//3)
g(n): number of irreducible fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2, gcd(a,b)=1

  The answer we should seek is g(12000).

f(n) = sigma{k=1, ..., n}(g(n//k))
-->
  g(n) = sigma{k=1, ..., n}μ(k)f(n//k)      [möbius inversion formula, μ(): möbius function]
       = sigma{k=1, ..., n}μ(k)sigma{j=1, ..., n//k}((j-1)//2 - j//3)
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func mobiusTbl(limit int) []int {
	pTbl := make([]int, limit+1)
	for i := range limit + 1 {
		pTbl[i] = i
	}

	for i := 2; i <= mylib.Isqrt(limit); i++ {
		if pTbl[i] == i {
			k := i * i
			for j := k; j <= limit; j += i {
				pTbl[j] = i
			}
			for j := k; j <= limit; j += k {
				pTbl[j] = 0
			}
		}
	}

	tbl := make([]int, limit+1)
	tbl[1] = 1
	for i := 2; i <= limit; i++ {
		if pTbl[i] != 0 {
			tbl[i] = -tbl[i/pTbl[i]]
		}
	}

	return tbl
}

func f(x int) int {
	var result int
	for j := 1; j <= x; j++ {
		result += (j-1)/2 - (j / 3)
	}

	return result
}

func g(N int) int {
	tbl := mobiusTbl(N)
	var result int
	for k := 1; k <= N; k++ {
		result += tbl[k] * f(N/k)
	}

	return result
}

func compute(limit int) string {
	return strconv.FormatInt(int64(g(limit)), 10)
}

func Solve() string {
	return compute(12_000)
}
