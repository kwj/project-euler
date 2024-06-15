package p0070

/*
  The answer must be a composite number because prime 'n' is not a permutation of phi(n) = n - 1.

  n = p1^k1 * p2^k2 * p3^k3 * ... * p{r}^k{n}
  -->
    phi(N) = N * (1-1/p1) * (1-1/p2) * (1-1/p3) * ... * (1-1/p{n})
      <-->
    N/phi(N) = (p1/(p1-1)) * (p2/(p2-1)) * (p3/(p3-1)) * ... * (p{n}/(p{n}-1))

  From the problem statement, 87109/phi(87109) = 87109 / 79180 = 1.1001.
  11/10 = 1.1 and 7/6 = 1.666..., so 11 <= prime numbers <= 9_999_999 / 11 = 909090.8181...

  The answer N has the following form (p_i are prime numbers)
    N = p1^k1 * p2^k2 * ... * pn^kn  (N < 10^7, n > 1, 11 <= p1 < p2 < ... < pn, k1>2 when n=1)

	--------
3137 * 3187
3137 * 3181
3137 * 3169
3137 * 3167
3137 * 3163
3137 * 3137
3121 * 3203
3121 * 3191
3121 * 3187
  ...

*/

import (
	"container/heap"
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

const LIMIT = 9_999_999

type pair struct {
	b int
	e int
}

func (p pair) values() (int, int) {
	return p.b, p.e
}

func prod(lst []pair) int {
	result := 1
	for _, tpl := range lst {
		result *= mylib.Pow(tpl.b, tpl.e)
	}

	return result
}

func phi(lst []pair) int {
	result := 1
	for _, tpl := range lst {
		result *= mylib.Pow(tpl.b, tpl.e-1) * (tpl.b - 1)
	}

	return result
}

func ratio(lst []pair) float64 {
	return float64(prod(lst)) / float64(phi(lst))
}

func pfGenerator(a, b int) chan []pair {
	aux := func(lst ...pair) []pair {
		b, e := lst[0].values()
		if tmp := LIMIT / prod(lst); tmp < b {
			return lst
		} else if prev_p := mylib.PrevPrime(tmp + 1); prev_p > b {
			return append([]pair{{prev_p, 1}}, lst...)
		} else {
			return append([]pair{{b, e + 1}}, lst[1:]...)
		}
	}

	ch := make(chan []pair, 1)

	// for safty
	if a > b {
		a, b = b, a
	}

	var pfLst []pair
	if a == b {
		pfLst = []pair{{a, 2}}
	} else {
		pfLst = []pair{{b, 1}, {a, 1}}
	}

	go func() {
		defer close(ch)

		for {
			b, e := pfLst[0].values()
			if len(pfLst) == 1 && e == 1 {
				break
			}

			result := slices.Clone(pfLst)
			slices.Reverse(result)

			if e > 1 {
				pfLst[0].e--
			} else {
				tmp_b, tmp_e := pfLst[1].values()
				if prevPrime := mylib.PrevPrime(b); prevPrime == tmp_b {
					pfLst = aux(append([]pair{{tmp_b, tmp_e + 1}}, pfLst[2:]...)...)
				} else {
					pfLst = aux(append([]pair{{prevPrime, 1}}, pfLst[1:]...)...)
				}
			}

			ch <- result
		}
	}()

	return ch
}

func isPermutation(a, b int) bool {
	counter := make([]int, 10) // 10 digit (0..=9)
	for a > 0 {
		counter[a%10]++
		a /= 10
	}
	for b > 0 {
		counter[b%10]--
		b /= 10
	}
	for _, x := range counter {
		if x != 0 {
			return false
		}
	}

	return true
}

func compute() string {
	primes := mylib.Primes(11, mylib.Isqrt(LIMIT))
	slices.Reverse(primes)

	// 'priorityQueue' and 'permItem' are defined in the pqueue_intf.go file.
	pq := make(priorityQueue, 1)
	pq[0] = &item{n: 87109, priority: float64(87109) / float64(79180), index: 0}
	heap.Init(&pq)

	for _, p := range primes {
		if ratio([]pair{{p, 1}}) > pq[0].priority {
			break
		}

		ch := pfGenerator(p, mylib.PrevPrime(LIMIT/p+1))
		for pfLst := range ch {
			if ratio(pfLst[0:min(len(pfLst), 2)]) > pq[0].priority {
				break
			}

			if n, totient := prod(pfLst), phi(pfLst); isPermutation(n, totient) {
				heap.Push(&pq, &item{n: n, priority: float64(n) / float64(totient)})
			}
		}
	}

	return strconv.FormatInt(int64(pq[0].n), 10)
}

func Solve() string {
	return compute()
}
