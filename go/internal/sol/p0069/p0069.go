package p0069

/*
    n/phi(n) = n / n(1-1/p{1})(1-1/p{2})...(1-1/p{r})
             = p{1}p{2}...p{r} / (p{1}-1)(p{2}-1)...(p{r}-1)
                    (p{i} is prime number)

  the above show that value of n/phi(n) depends on the prime factors of 'n'.

  generally, 1 < m/(m-1) and m/(m-1) > n/(n-1) [m<n].

  so I'll find the maximum 'k' which satisfies follwing condition.

    p{1} * p{2} * ... * p{k-1} * p{k} <= 1_000_000
       [p{i} is prime number: 2, 3, 5, 7, ...]

  the answer 'n' is p{1} * p{2} * ... * p{k-1} * p{k}.
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(limit int) string {
	pGen := mylib.PrimeGenerator{}
	var prime int
	result := 1
	for {
		prime = pGen.Next()
		if result*prime > limit {
			break
		}
		result *= prime
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000_000)
}
