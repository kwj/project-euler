package p0085

/*
  nCr = n! / ((n-r)! * r!)

      1  2       n-1  n
    +--+--+-- ... --+--+
   1|  |  |   ...   |  |
    +--+--+-- ... --+--+
   2|  |  |   ...   |  |
    +--+--+-- ... --+--+ num of horizontal lines = m + 1
   3|  |  |   ...   |  |
    +--+--+-- ... --+--+
    ....................
    +--+--+-- ... --+--+
   m|  |  |   ...   |  |
    +--+--+-- ... --+--+
      num of vertical lines = n + 1

  (m+1)C2 * (n+1)C2 = m(m+1)/2 * n(n+1)/2 (\approx) 2_000_000
  --> m(m+1)*n(n+1) (\approx) 8_000_000
*/

import (
	"container/heap"
	"strconv"

	"pe-solver/internal/mylib"
)

func getDiff(m, target int) (int, int, bool) {
	lhs := func(m, n int) int { return m * (m + 1) * n * (n + 1) }

	n := mylib.Isqrt(target/(m*(m+1))) - 1
	var diff1 int
	var diff2 int
	for {
		if tmp := target - lhs(m, n); tmp > 0 {
			diff1 = tmp
			n++
		} else {
			diff2 = tmp
			break
		}
	}
	if m >= n {
		return 0, 0, false
	}

	diff1, diff2 = mylib.Abs(diff1), mylib.Abs(diff2)
	if diff1 < diff2 {
		return diff1, m * (n - 1), true
	} else {
		return diff2, m * n, true
	}
}

func compute(target int) string {
	// 'priorityQueue' and 'item' are defined in the pqueue_intf.go file.
	pq := make(priorityQueue, 0)
	heap.Init(&pq)

	target *= 4
	for m := 1; ; m++ {
		if diff, area, ok := getDiff(m, target); ok {
			heap.Push(&pq, &item{area: area, priority: diff})
		} else {
			break
		}
	}

	return strconv.FormatInt(int64(pq[0].area), 10)
}

func Solve() string {
	return compute(2_000_000)
}
