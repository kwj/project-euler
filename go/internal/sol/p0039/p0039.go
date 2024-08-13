package p0039

/*
Primitive Pythagorean triples (variant type)
  https://en.wikipedia.org/wiki/Pythagorean_triple#A_variant

`m` > `n` > 0.
`m` and `n` are both odd and coprime.
   hypotenuse: (m^2 + n^2) / 2
   catheti: mn, (m^2 - n^2) / 2
   perimeter: mn + (m^2 - n^2) / 2 + (m^2 + n^2) / 2 = m(m + n)
*/

import (
	"maps"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(limit int) string {
	lst := make([]int, 0)
	for m := 3; m <= mylib.Isqrt(limit); m += 2 {
		for n := 1; n < m; n += 2 {
			if mylib.Gcd(m, n) == 1 {
				p := m * (m + n)
				for perimeter := p; perimeter <= limit; perimeter += p {
					lst = append(lst, perimeter)
				}
			}
		}
	}

	var maxCnt int
	var result int
	for k, cnt := range maps.All(mylib.Frequencies(lst)) {
		if cnt > maxCnt {
			result = k
			maxCnt = cnt
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000)
}
