package p0061

import (
	"pe-solver/internal/mylib"
	"strconv"
)

// nextHop: mapping table for next hop
//
// key: first two digis of 4-digit number
// value: array of last two digit of 4-digit number
//
// --------
// Example of 4-digit triangle numbers:
// 1013, 1081, 1120, 1176, 1225, 1275, ..., 2556, 2628, 2701, 2775, 2850, 2926, ...
// -->
// map[10:[35 81], 11:[28 76], 12:[25 75], ..., 25:[56], 26:[28], 27:[75], 28:[50], 29:[26], ...
//
// Note:
// The last two digit of *2701* is less than 10 and it shows that no next hop exists.
// Thus, this number doesn't contain as an entry in a guide map.
type nextHop map[int][]int

func polygonalTbl() map[int]nextHop {
	fn := map[int]func(int) int{
		3: func(n int) int { return n * (n - 1) / 2 },
		4: func(n int) int { return n * n },
		5: func(n int) int { return n * (3*n - 1) / 2 },
		6: func(n int) int { return n * (2*n - 1) },
		7: func(n int) int { return n * (5*n - 3) / 2 },
		8: func(n int) int { return n * (3*n - 2) },
	}
	tbl := map[int]nextHop{}

	for i := 3; i <= 8; i++ {
		m := nextHop{}
		for j := 1; ; j++ {
			n := fn[i](j)
			if n < 1_000 {
				continue
			} else if n >= 10_000 {
				break
			} else if n%100 < 10 {
				continue
			} else {
				k := n / 100
				v := n % 100
				if x := m[k]; x != nil {
					m[k] = append(x, v)
				} else {
					m[k] = []int{v}
				}
			}
		}
		tbl[i] = m
	}

	return tbl
}

// From the problem statement, there is the only ordered set which satisfies the conditions.
// So, if it is found, searching can be terminated.
//
// If the return value is nil, the circular route specified in the guide does not exist.
func findCircularRoute(tbl map[int]nextHop, start int, guide []int) []int {
	isDistinctNumbers := func(xs []int) bool {
		m := map[int]struct{}{}
		for i := 0; i < len(xs)-1; i++ {
			m[xs[i]*100+xs[i+1]] = struct{}{}
		}

		return len(m) == len(xs)-1
	}

	var dfs func([]int, []int) []int
	dfs = func(guide, route []int) []int {
		if len(guide) == 0 {
			if route[0] == route[len(route)-1] && isDistinctNumbers(route) {
				return route[1:]
			} else {
				return nil
			}
		}
		nextMap := tbl[guide[0]]
		if nextCands, ok := nextMap[route[0]]; !ok {
			return nil
		} else {
			for _, nextNum := range nextCands {
				if result := dfs(guide[1:], append([]int{nextNum}, route...)); result != nil {
					return result
				}
			}
		}

		return nil
	}

	for k, v := range tbl[start] {
		for _, nextNum := range v {
			if result := dfs(guide, []int{nextNum, k}); result != nil {
				return result
			}
		}
	}

	return nil
}

func compute() string {
	sum := func(xs []int) int {
		var result int
		for _, v := range xs {
			result += v
		}

		return result
	}

	tbl := polygonalTbl()

	// start with octagonal numbers
	ch := mylib.Permutations([]int{7, 6, 5, 4, 3}, 5)
	for guide := range ch {
		if route := findCircularRoute(tbl, 8, guide); route != nil {
			// (100*route[0] + route[1]) + (100*route[1] + route[2]) + ... + (100*route[n] + route[0])
			//   = sum(route) * 101
			return strconv.FormatInt(int64(sum(route)*101), 10)
		}
	}

	panic("no circular route exists")
}

func Solve() string {
	return compute()
}
