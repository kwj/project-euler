package p0071

/*
Farey sequence

 2/5, 3/7
   -> 2/5, (2+3)/(5+7), 3/7
   -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), 3/7
   -> 2/5, (2+3)/(5+7), (2+3+3)/(5+7+7), (2+3+3+3)/(5+7+7+7), 3/7
    ...
   -> 2/5, ..., (2+3x)/(5+7x), 3/7

     5+7x <= 1_000_000
*/

import (
	"strconv"
)

func compute(limit int) string {
	return strconv.FormatInt(int64(2+3*((limit-5)/7)), 10)
}

func Solve() string {
	return compute(1_000_000)
}
