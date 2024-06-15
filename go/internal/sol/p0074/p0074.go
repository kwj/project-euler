package p0074

import (
	"strconv"
)

func nextNode(n int) int {
	// 0!, 1!, ..., 9!
	tbl := [...]int{1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880}
	if n == 0 {
		return tbl[0]
	}

	var result int
	for n > 0 {
		result += tbl[n%10]
		n /= 10
	}

	return result
}

// https://en.wikipedia.org/wiki/Cycle_detection
//
// I think this is a slow method to this problem, however, I wanted to try this.
func cycleDetectionByFloyd(n int, cacheTbl []int) int {
	slow, fast := n, n

	// step 1
	slow = nextNode(slow)
	fast = nextNode(nextNode(fast))
	cnt := 1
	for slow != fast {
		if slow < len(cacheTbl) && cacheTbl[slow] != 0 {
			return cnt + cacheTbl[slow]
		}

		slow = nextNode(slow)
		fast = nextNode(nextNode(fast))
		cnt++
	}

	// step 2
	mu := 0
	fast = n
	for slow != fast {
		slow = nextNode(slow)
		fast = nextNode(fast)
		mu++
	}

	// step 3
	lam := 1
	fast = nextNode(slow)
	for slow != fast {
		fast = nextNode(fast)
		lam++
	}

	return mu + lam
}

func compute(limit int) string {
	cacheTbl := make([]int, limit)
	const chainLength = 60

	var result int
	for n := 1; n < limit; n++ {
		steps := cycleDetectionByFloyd(n, cacheTbl)
		cacheTbl[n] = steps
		if steps == chainLength {
			result++
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000_000)
}
