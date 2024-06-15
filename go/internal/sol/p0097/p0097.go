package p0097

import (
	"fmt"

	"pe-solver/internal/mylib"
)

const modulo = 10_000_000_000

func compute() string {
	return fmt.Sprintf("%010d", (28433*mylib.PowerMod(2, 7830457, modulo)+1)%modulo)
}

func Solve() string {
	return compute()
}
