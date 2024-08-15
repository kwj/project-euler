package mylib

import (
	"math/big"
)

type PrimeGenerator struct {
	n   int
	tbl map[int]int
}

func (pg *PrimeGenerator) Next() int {
	if pg.n == 0 {
		pg.n = 1
		pg.tbl = make(map[int]int)
		return 2
	}

	for {
		pg.n += 2
		factor, isComposite := pg.tbl[pg.n]
		if isComposite {
			delete(pg.tbl, pg.n)
		} else {
			factor = pg.n * 2
		}

		for i := pg.n + factor; ; i += factor {
			_, checked := pg.tbl[i]
			if !checked {
				pg.tbl[i] = factor
				break
			}
		}

		if !isComposite {
			return pg.n
		}
	}
}

/*
32-bit integer witnesses for Miller-Rabin primality test
ref: http://ceur-ws.org/Vol-1326/020-Forisek.pdf (FJ32_256)
*/
func sprpBase(n uint32) int {
	var bases = [...]int{
		15591, 2018, 166, 7429, 8064, 16045, 10503, 4399, 1949, 1295, 2776, 3620, 560, 3128, 5212, 2657,
		2300, 2021, 4652, 1471, 9336, 4018, 2398, 20462, 10277, 8028, 2213, 6219, 620, 3763, 4852, 5012,
		3185, 1333, 6227, 5298, 1074, 2391, 5113, 7061, 803, 1269, 3875, 422, 751, 580, 4729, 10239,
		746, 2951, 556, 2206, 3778, 481, 1522, 3476, 481, 2487, 3266, 5633, 488, 3373, 6441, 3344,
		17, 15105, 1490, 4154, 2036, 1882, 1813, 467, 3307, 14042, 6371, 658, 1005, 903, 737, 1887,
		7447, 1888, 2848, 1784, 7559, 3400, 951, 13969, 4304, 177, 41, 19875, 3110, 13221, 8726, 571,
		7043, 6943, 1199, 352, 6435, 165, 1169, 3315, 978, 233, 3003, 2562, 2994, 10587, 10030, 2377,
		1902, 5354, 4447, 1555, 263, 27027, 2283, 305, 669, 1912, 601, 6186, 429, 1930, 14873, 1784,
		1661, 524, 3577, 236, 2360, 6146, 2850, 55637, 1753, 4178, 8466, 222, 2579, 2743, 2031, 2226,
		2276, 374, 2132, 813, 23788, 1610, 4422, 5159, 1725, 3597, 3366, 14336, 579, 165, 1375, 10018,
		12616, 9816, 1371, 536, 1867, 10864, 857, 2206, 5788, 434, 8085, 17618, 727, 3639, 1595, 4944,
		2129, 2029, 8195, 8344, 6232, 9183, 8126, 1870, 3296, 7455, 8947, 25017, 541, 19115, 368, 566,
		5674, 411, 522, 1027, 8215, 2050, 6544, 10049, 614, 774, 2333, 3007, 35201, 4706, 1152, 1785,
		1028, 1540, 3743, 493, 4474, 2521, 26845, 8354, 864, 18915, 5465, 2447, 42, 4511, 1660, 166,
		1249, 6259, 2553, 304, 272, 7286, 73, 6554, 899, 2816, 5197, 13330, 7054, 2818, 3199, 811,
		922, 350, 7514, 4452, 3449, 2663, 4708, 418, 1621, 1171, 3471, 88, 11345, 412, 1559, 194,
	}

	h := uint64(n)
	h = ((h >> 16) ^ h) * 0x45d9f3b
	h = ((h >> 16) ^ h) * 0x45d9f3b
	h = ((h >> 16) ^ h) & 0xff

	return bases[h]
}

func millerRabinTest(n, base int) bool {
	d := n - 1
	s := big.NewInt(int64(d)).TrailingZeroBits()
	d >>= s
	x := PowerMod(base, d, n)

	if x == 1 || x == n-1 {
		return true
	}

	for range s - 1 {
		x = PowerMod(x, 2, n)
		if x == n-1 {
			return true
		}
	}

	return false
}

/*
Primality test for int64 (n <= 2^63)

True: prime (No composite number below 2^64 passes the Baillie-PSW primality test)
False: composite number

n <= UINT16: Lookup table
n <= UINT32: Trial division and Miller-Rabin primality test [deterministic variant using hash function]
n > UINT32: Trial division and Baillie-PSW primality test [using math/big.ProbablyPrime()]
*/
func IsPrime(n int) bool {
	// Even numbers (except 2) are composite numbers.
	if n&1 == 0 {
		return n == 2
	}

	// maxUINT16 and minFactorTbl are defined in 'factor.go'.
	if n <= maxUINT16 {
		if n < 2 {
			return false
		} else {
			return minFactorTbl[n>>1] == 1
		}
	}

	// trial division
	for _, x := range [...]int{3, 5, 7, 11, 13, 17, 19, 23, 29, 31} {
		if n%x == 0 {
			return false
		}
	}

	// maxUINT32 is defined in 'factor.go'.
	if n <= maxUINT32 {
		return millerRabinTest(n, sprpBase(uint32(n)))
	}

	// ProbablyPrime(0) applies only a Baillie-PSW test. (since Go 1.18)
	return big.NewInt(int64(n)).ProbablyPrime(0)

}

func numToIndex(n int) int {
	// index table of indicating prime number candidates
	// ranges: [2, 211], [213, 411], ...
	var indices = [...]int{
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 2,
		2, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6,
		7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 10, 10, 10,
		10, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 13, 13,
		14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 16, 16, 17, 17, 17,
		17, 17, 17, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 20, 20,
		20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 22, 22, 23, 23, 23,
		23, 24, 24, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26,
		27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 29, 29, 29, 29, 29,
		29, 30, 30, 31, 31, 31, 31, 32, 32, 32, 32, 32, 32, 33, 33,
		34, 34, 34, 34, 34, 34, 35, 35, 35, 35, 35, 35, 36, 36, 36,
		36, 37, 37, 38, 38, 38, 38, 39, 39, 39, 39, 39, 39, 40, 40,
		41, 41, 41, 41, 41, 41, 42, 42, 42, 42, 43, 43, 44, 44, 44,
		44, 45, 45, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 47, 47,
	}

	if n < 2 {
		return 0
	}
	n -= 2

	// 2*3*5*7 = 210 = len(indices)
	// 48: number of prime number candidates in each ranges (range size = 210)
	return 48*(n/(2*3*5*7)) + indices[n%(2*3*5*7)]
}

func indexToNum(idx int) int {
	if idx < 0 {
		panic("index must not be negative")
	}
	var wheelPrimeCandidates = [...]int{
		11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
		73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 121, 127, 131, 137, 139, 143,
		149, 151, 157, 163, 167, 169, 173, 179, 181, 187, 191, 193, 197, 199, 209, 211,
	}

	q := idx / 48
	r := idx % 48
	return (2*3*5*7)*q + wheelPrimeCandidates[r]
}

func NextPrime(n int) int {
	switch {
	case n < 2:
		return 2
	case n < 3:
		return 3
	case n < 5:
		return 5
	case n < 7:
		return 7
	default:
		idx := numToIndex(n)
		if numToIndex(n+1) != idx {
			idx += 1
		}
		for {
			p := indexToNum(idx)
			if IsPrime(p) {
				return p
			}
			idx += 1
		}
	}
}

func PrevPrime(n int) int {
	if n <= 2 {
		panic("argument must be grater than 2")
	}
	switch {
	case n <= 3:
		return 2
	case n <= 5:
		return 3
	case n <= 7:
		return 5
	case n <= 11:
		return 7
	default:
		idx := numToIndex(n) - 1
		for {
			p := indexToNum(idx)
			if IsPrime(p) {
				return p
			}
			idx -= 1
		}
	}
}

var wheelGaps = [...]int{
	2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4, 2,
	6, 4, 6, 8, 4, 2, 4, 2, 4, 8, 6, 4, 6, 2, 4, 6,
	2, 6, 6, 4, 2, 4, 6, 2, 6, 4, 2, 4, 2, 10, 2, 10,
}

func getSmallPrimeTbl(upper int) []bool {
	if upper < 11 {
		panic("upper must be equal or grater than 11")
	}
	maxIndex := numToIndex(upper+1) - 1
	maxPrime := indexToNum(maxIndex)
	tbl := make([]bool, maxIndex+1)
	for i := range tbl {
		tbl[i] = true
	}

	if upper >= 121 { // 121 = 11 * 11
		for i := range Isqrt(upper) + 1 {
			if tbl[i] {
				prime := indexToNum(i)
				idx := i % len(wheelGaps)
				q := prime * prime
				for q <= maxPrime {
					tbl[numToIndex(q)] = false
					q += prime * wheelGaps[idx]
					idx = (idx + 1) % len(wheelGaps)
				}
			}
		}
	}

	return tbl
}

func getPrimeTbl(low, high int) []bool {
	if low > high || low < 11 {
		panic("range error")
	}
	if low == 11 {
		return getSmallPrimeTbl(high)
	}

	wLow := numToIndex(low)
	wHigh := numToIndex(high+1) - 1
	maxPrime := indexToNum(wHigh)
	tbl := make([]bool, wHigh-wLow+1)
	for i := range tbl {
		tbl[i] = true
	}

	if high >= 121 { // 121 = 11 * 11
		for i, flag := range getSmallPrimeTbl(Isqrt(high)) {
			if flag {
				prime := indexToNum(i)
				idx := numToIndex(max((low+prime-1)/prime, prime))
				q := prime * indexToNum(idx)
				for q <= maxPrime {
					tbl[numToIndex(q)-wLow] = false
					idx %= len(wheelGaps)
					q += prime * wheelGaps[idx]
					idx += 1
				}
			}
		}
	}

	return tbl
}

func Primes(args ...int) []int {
	low, high := 1, 2
	switch len(args) {
	case 1:
		if args[0] < 2 {
			panic("range error")
		}
		low, high = 1, args[0]
	case 2:
		low, high = args[0], args[1]
		if low < 1 || low > high {
			panic("range error")
		}
	default:
		panic("invalid arguments")
	}

	lst := make([]int, 0)
	if low <= 2 {
		lst = append(lst, 2)
	}
	if low <= 3 {
		lst = append(lst, 3)
	}
	if low <= 5 {
		lst = append(lst, 5)
	}
	if low <= 7 {
		lst = append(lst, 7)
	}
	if high >= 11 {
		low = max(low, 11)
		wOffset := numToIndex(low)
		for i, flag := range getPrimeTbl(low, high) {
			if flag {
				lst = append(lst, indexToNum(i+wOffset))
			}
		}
	}

	return lst
}

// divisor function
// https://en.wikipedia.org/wiki/Divisor_function
func SigmaTbl(z, upper int) []int {
	primes := Primes(upper)
	result := make([]int, upper+1)
	for i := range result {
		result[i] = 1
	}

	for _, p := range primes {
		q := p
		x := 0
		for q <= upper {
			x += Pow(q, z)
			result[q] += x
			q *= p
		}
	}

	for _, p := range primes {
		q := p
		for q <= upper {
			for n := 2; n <= upper/q; n++ {
				if result[n] == 1 || n%p == 0 {
					continue
				}
				result[q*n] = result[q] * result[n]
			}
			q *= p
		}
	}

	return result
}
