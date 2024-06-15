package p0057

/*
use recurrence relation:
  sqrt(2) = 1 + sqrt(2) - 1
          = 1 + 1 / ( 1 / (sqrt(2) - 1) )
          = 1 + 1 / ( (sqrt(2) + 1) / (2 - 1) )
          = 1 + 1 / (1 + sqrt(2))
  -->
  a{1} = 1 + 1/2
  a{n} = 1 + 1/(1 + a{n-1})    [n>1]

assume that b{n}/c{n} = a{n}
  b{1}/c{1} = 1 + 1/2 = 3/2
  b{n}/c{n} = 1 + 1/(1 + b{n-1}/c{n-1})
            = 1 + 1/((c{n-1) + b{n-1})/c{n-1})
            = 1 + c{n-1}/(c{n-1) + b{n-1})
            = (c{n-1) + b{n-1} + c{n-1))/(c{n-1) + b{n-1})
            = (2 * c{n-1} + b{n-1}) / (c{n-1) + b{n-1}
*/

import (
	"math/big"
	"strconv"
)

func compute(limit int) string {
	var result int
	b := big.NewInt(1)
	c := big.NewInt(1)

	for range limit {
		b.Add(c, b)
		c.Add(c, b)
		b, c = c, b
		if len(b.String()) > len(c.String()) {
			result++
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000)
}
