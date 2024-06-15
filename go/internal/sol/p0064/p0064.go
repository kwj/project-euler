package p0064

/*
            sqrt(N) + b0        1              1
  sqrt(N) = ------------ = a0 + --,  x1 = a1 + --, ...
                 c0             x1             x2

                  c0             c0(sqrt(N) - (b0 - a0c0))
    x1 = --------------------- = -------------------------
         sqrt(N) + (b0 - a0c0)       N - (b0 - a0c0)^2

         sqrt(N) + (a0c0 - b0)   sqrt(N) + b1         1
       = --------------------- = ------------- = a1 + --
           N - (a0c0 - b0)^2          c1              x2
           -----------------
                  c0
   -->
     a{n} = floor( (sqrt(N)+b{n}) / c{n} )
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}

     b{0} = 0, c{0} = 1, a{0} = sqrt(N)
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func contFraction(n int) (int, []int) {
	isqrt_n := mylib.Isqrt(n)
	if isqrt_n*isqrt_n == n {
		return isqrt_n, []int{}
	}

	sentinel := 2 * isqrt_n
	b := 0
	c := 1
	a := (isqrt_n + b) / c
	result := make([]int, 0)

	for {
		b = a*c - b
		c = (n - b*b) / c
		a = (isqrt_n + b) / c
		result = append(result, a)
		if a == sentinel {
			return isqrt_n, result
		}
	}
}

func compute(limit int) string {
	var result int
	for n := 1; n <= limit; n++ {
		if _, lst := contFraction(n); len(lst)&1 == 1 {
			result++
		}

	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(10_000)
}
