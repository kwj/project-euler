package p0088

/*
  N(k) = a1 + a2 + ... + ak = a1 * a2 * ... * ak
    N(k) must be composite numbers.
  min_N(k): minimal product-sum N(k)

  when k = 2
    sum {2,2} = prod {2,2}
  when k > 2 and {a1, a2, a3, ..., ak}
    min sum = sum {1, 1, ..., 1} = k
    --> min_N(k) >= k
  when k > 2 and {a1, a2, a3, ..., ak} = {1, ..., 1, 2, k}
    for all k>2, there exists Ak = {a1, a2, ..., ak} = {1, 1, ..., 1, 2, k}, and
    prod Ak = sum Ak = N(k) = 2k
    --> min_N(k) <= 2k

  2 <= k <= 12000
  --> k <= N(k) <= 24000
*/

import (
	"slices"
	"strconv"
)

func aux(p, s, length, n, limit int, tbl []int) {
	k := p - s + length
	if k > limit {
		return
	}
	if p < tbl[k] {
		tbl[k] = p
	}

	nextLength := length + 1
	for x := n; x <= (limit*2)/p; x++ {
		aux(p*x, s+x, nextLength, x, limit, tbl)
	}
}

func compute(limit int) string {
	tbl := make([]int, limit+1)
	for i := range len(tbl) {
		tbl[i] = limit * 2
	}

	aux(1, 0, 0, 2, limit, tbl)

	type intSet map[int]struct{}
	nSet := intSet{}
	for v := range slices.Values(tbl[2:]) {
		nSet[v] = struct{}{}
	}

	var result int
	for v := range nSet {
		result += v
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(12_000)
}
