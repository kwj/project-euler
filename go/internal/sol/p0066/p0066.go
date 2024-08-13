package p0066

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
      b{n+1} = a{n}*c{n} - b{n}
      c{n+1} = (N - b{n+1}^2) / c{n}
      a{n+1} = floor((sqrt(N)+b{n+1}) / c{n+1}) = (floor(sqrt(N)) + b{n+1}) / c{n+1}

        a{0} = floor(sqrt(N)), b{0} = 0, c{0} = 1


   write A{0}, A{1}, A{2}, ...
            x{0}                          x{1}                      1         x{2}
     a{0} = ---- = A{0},  a{0} + 1/a{1} = ---- = A{1},  a{0} + ------------ = ---- = A{2},  ...
            y{0}                          y{1}                         1      y{2}
                                                               a{1} + ----
                                                                      a{2}
                                               [x{0} = a{0}, y{0} = 1]
    -->
         n=0: -> x{0} = a{0}, y{0} = 1
         n=1: -> x{1} = a{0}*a{1} + 1, y{1} = a{1}

         n=2: -> x{2}/y{2}
                      = (a{0}*a{1}*a{2} + a{0} + a{2}) / (a{1}a{2} + 1)

                        a{2}*(a{0}*a{1} + 1) + a{0}   a{2}*x{1} + a{0}
                      = --------------------------- = ----------------
                             a{2}*a{1} + 1            a{2}*y(1) + 1

                        a{2}*x{1} + x{0}
                      = ----------------
                        a{2)*y{1} + y{0}

                                     a{k}*x{k-1} + x{k-2}
      assume that A{k} = x{k}/y{k} = --------------------  [k>=2]
                                     a{k}*y{k-1} + y{k-2}

                                  ((a{k}*a{k+1} + 1)/a{k+1})*x{k-1} + x{k-2}
         A{k+1} = x{k+1}/y{k+1} = -----------------------------------------
                                  ((a{k}*a{k+1} + 1)/a{k+1})*y{k-1} + y{k-2}

                                  (a{k}*a{k+1} + 1)*x{k-1} + x{k-2}*a{k+1}
                                = -----------------------------------------
                                  (a{k}*a{k+1} + 1)*y{k-1} + y{k-2}*a{k+1}

                                  a{k+1}(a{k}*x{k-1} + x{k-2}) + x{k-1}
                                = -------------------------------------
                                  a{k+1}(a{k}*y{k-1} + y{k-2}) + y{k-1}

                                  a{k+1}*x{k} + x{k-1}
                                = --------------------
                                  a{k+1}*y{k} + y{k-1}
       -->
         x{k+1} = a{k+1} * x{k} + x{k-1}
         y{k+1} = a{k+1} * y{k} + y{k-1}

    -->
      [a{0}; a{1], a{2}, ...]
      assume that x{-1} = 1, x{0} = a{0}, y{-1} = 0, y{0} = 1

      [n>=1]
        x{n} = a{n} * x{n-1} + x{n-2}
        y{n} = a{n} * y{n-1} + y{n-2}
*/

import (
	"math/big"
	"slices"
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

func numerator(a0 int, lst []int) *big.Int {
	n1, n2 := big.NewInt(int64(a0)), big.NewInt(1)
	for a := range slices.Values(lst) {
		tmp := big.NewInt(int64(a))
		tmp.Mul(tmp, n1)
		tmp.Add(tmp, n2)
		n1, n2 = tmp, n1
	}

	return n1
}

func compute(limit int) string {
	maxNumerator := big.NewInt(0)
	var result int

	for i := 1; i <= limit; i++ {
		cfA0, cfLst := contFraction(i)
		if len(cfLst) == 0 {
			continue
		}
		if len(cfLst)&1 == 1 {
			cfLst = slices.Concat(cfLst, cfLst)
		}

		num := numerator(cfA0, cfLst[:len(cfLst)-1])
		if maxNumerator.Cmp(num) < 0 {
			maxNumerator = num
			result = i
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000)
}
