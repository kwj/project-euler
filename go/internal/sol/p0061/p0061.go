package p0061

import (
	"slices"
	"strconv"
)

// state.bits
//
//	0b######000
//	  ||||||
//	  |||||+- triangle
//	  ||||+-- square
//	  |||+--- pentagonal
//	  ||+---- hexagonal
//	  |+----- heptagonal
//	  +------ octagonal
type state struct {
	bits int
	path []int
}

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

func findClosedPaths() [][]int {
	var paths [][]int
	tbl := polygonalTbl()

	getNextStates := func(st state) []state {
		var states []state

		if st.bits == 0b111111000 && st.path[0] == st.path[len(st.path)-1] {
			paths = append(paths, st.path)
		} else {
			for i := 3; i < 8; i++ {
				p_bit := 0b1 << i
				if st.bits&p_bit != 0 {
					continue
				}
				next_tbl := tbl[i]
				if vs, ok := next_tbl[st.path[len(st.path)-1]]; ok {
					for x := range slices.Values(vs) {
						states = append(states, state{bits: st.bits | p_bit, path: append(append([]int{}, st.path...), x)})
					}
				}
			}
		}

		return states
	}

	// Search by BFS (start from octagonal numbers)
	var q []state
	for k, vs := range tbl[8] {
		for v := range slices.Values(vs) {
			q = append(q, state{bits: 0b1 << 8, path: []int{k, v}})
		}
	}
	for len(q) > 0 {
		var st state
		st, q = q[0], q[1:]
		for next_st := range slices.Values(getNextStates(st)) {
			q = append(q, next_st)
		}
	}

	return paths
}

func compute() string {
	isDistinctNumbers := func(xs []int) bool {
		m := map[int]struct{}{}
		for i := 0; i < len(xs)-1; i++ {
			m[xs[i]*100+xs[i+1]] = struct{}{}
		}

		return len(m) == len(xs)-1
	}

	sum := func(xs []int) int {
		var result int
		for v := range slices.Values(xs) {
			result += v
		}

		return result
	}

	// All numbers in a cycle are different numbers
	var cycles [][]int
	for lst := range slices.Values(findClosedPaths()) {
		if isDistinctNumbers(lst) {
			cycles = append(cycles, lst)
		}
	}

	// There exists only one cycle
	if len(cycles) == 1 {
		// cycles[0] = lst: [d1, d2, d3, d4, d5, d6, d1]
		//
		// (100*lst[0] + lst[1]) + (100*lst[1] + lst[2]) + ... + (100*lst[5] + lst[6])
		//   = (100 * d1 + d2) + (100 * d2 + d1) + ... + (100 * d6 + d1)
		//   = sum(lst[1:]) * 101
		return strconv.FormatInt(int64(sum(cycles[0][1:])*101), 10)
	}

	panic("no circular route exists")
}

func Solve() string {
	return compute()
}
