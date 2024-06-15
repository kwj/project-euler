package p0068

/*
  ring:

          r1
            \
            r0  [r6 <- r0]
           /  \
         r4---r2-r3
         /
       r5

          r1
            \  [r10 <- r0]
            r0  r3
           /  \ /
         r8   r2
         /\   /
      r9 r6-r4-r5
            \
             r7
*/

import (
	"fmt"
	"slices"
)

func isValid(nGon, x, y int, bitMask uint) bool {
	if x <= 0 || x > nGon*2 {
		return false
	}
	if y <= 0 || y > nGon*2 {
		return false
	}
	if x == y {
		return false
	}
	if ((1<<x)|(1<<y))&bitMask != 0 {
		return false
	}

	return true
}

/*
 * bitMask: 1 - used number, 0 - unused number
 * example: 5-gon ring
 * 0x11111111110
 *   ^        ^
 *  10        1
 */
func dfs(nGon, idx, total int, bitMask uint, ring []int, result *[]string) {
	const startPos = 1
	if idx == nGon*2-2 {
		tmp := total - ring[0] - ring[idx]
		if 0 < tmp && tmp <= nGon*2 && tmp > ring[startPos] && (1<<tmp)&bitMask == 0 {
			ring[idx+1] = tmp
			var s string
			for {
				s = fmt.Sprintf("%d%d%d%s", ring[idx+1], ring[idx], ring[idx+2], s)
				if idx == 0 {
					break
				}
				idx -= 2
			}
			*result = append(*result, s)
		}
	} else {
		for outer := 1; outer <= nGon*2; outer++ {
			inner := total - ring[idx] - outer
			if !isValid(nGon, outer, inner, bitMask) {
				continue
			}
			ring[idx+1] = outer
			ring[idx+2] = inner

			if ring[startPos] <= outer {
				dfs(nGon, idx+2, total, (1<<outer)|(1<<inner)|bitMask, ring, result)
			}
		}
	}
}

func compute(nGon int) string {
	ring := make([]int, nGon*2+1)
	var result []string

	for total := (nGon * 2) + 3; total <= nGon*4; total++ {
		for n := 1; n <= nGon*2; n++ {
			ring[0] = n
			ring[nGon*2] = n
			dfs(nGon, 0, total, (1 << n), ring, &result)
		}
	}

	// When nGon is equal to 5, only 16-digit strings are correctly answers in this problem.
	if nGon == 5 {
		result = slices.DeleteFunc(result, func(s string) bool { return len(s) != 16 })
	}

	slices.Sort(result)

	return result[len(result)-1]
}

func Solve() string {
	return compute(5)
}
