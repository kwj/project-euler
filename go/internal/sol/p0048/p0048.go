package p0048

import (
	"fmt"

	"pe-solver/internal/mylib"
)

func compute(upper int) string {
	var modulus = mylib.Pow(10, 10)

	var result int
	for n := 1; n <= upper; n++ {
		result = (result + mylib.PowerMod(n, n, modulus)) % modulus
	}

	return fmt.Sprintf("%010d", result)
}

func Solve() string {
	return compute(1_000)
}
