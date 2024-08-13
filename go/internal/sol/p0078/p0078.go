package p0078

/*
  https://en.wikipedia.org/wiki/Partition_(number_theory)
  https://en.wikipedia.org/wiki/Partition_function_(number_theory)
  https://en.wikipedia.org/wiki/Pentagonal_number_theorem

  p(n) = Sigma{k ∈ Z/{0}} (-1)^(k+1) * p(n - k(3k-1)/2)
       = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-12) + p(n-15) - p(n-22) - ...

    [p(0) = 1, p(k) = 0 when k < 0]
*/

import (
	"slices"
	"strconv"
)

type gpnumGenerator struct {
	gap  int
	step int
	flag int
	acc  int
}

func (gp *gpnumGenerator) next() int {
	if gp.flag == 0 {
		gp.acc += gp.gap
		gp.gap += 2
	} else {
		gp.acc += gp.step
		gp.step += 1
	}
	gp.flag = (gp.flag + 1) & 1

	return gp.acc
}

func compute(denom int) string {
	gpGen := gpnumGenerator{gap: 1, step: 1, flag: 0, acc: 0}
	gp := []int{gpGen.next()}
	p := []int{1}
	n := 1

	for {
		if n > gp[len(gp)-1] {
			gp = append(gp, gpGen.next())
		}
		rem := 0
		for i, x := range slices.All(gp) {
			if x > n {
				break
			}

			if i%4 < 2 {
				rem += p[n-x]
			} else {
				rem -= p[n-x]
			}
		}

		rem %= denom
		if rem == 0 {
			break
		}
		p = append(p, rem)
		n++
	}

	return strconv.FormatInt(int64(n), 10)
}

func Solve() string {
	return compute(1_000_000)
}
