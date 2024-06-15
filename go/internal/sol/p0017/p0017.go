package p0017

import (
	"strconv"
)

var under20 = []int{
	len(""),
	len("one"),
	len("two"),
	len("three"),
	len("four"),
	len("five"),
	len("six"),
	len("seven"),
	len("eight"),
	len("nine"),
	len("ten"),
	len("eleven"),
	len("twelve"),
	len("thirteen"),
	len("fourteen"),
	len("fifteen"),
	len("sixteen"),
	len("seventeen"),
	len("eighteen"),
	len("nineteen"),
}

var mults10 = []int{
	len(""),
	len(""),
	len("twenty"),
	len("thirty"),
	len("forty"),
	len("fifty"),
	len("sixty"),
	len("seventy"),
	len("eighty"),
	len("ninety"),
}

func countLetters(n int) int {
	if n <= 0 || n > 1000 {
		panic("range error")
	}

	switch {
	case n == 1000:
		return under20[1] + len("thousand")
	case n < 20:
		return under20[n]
	case n < 100:
		return mults10[n/10] + under20[n%10]
	default:
		q, r := n/100, n%100
		if r == 0 {
			return under20[q] + len("hundred")
		} else {
			return under20[q] + len("hundred") + len("and") + countLetters(r)
		}
	}
}

func compute(limit int) string {
	var result int
	for i := 1; i <= limit; i++ {
		result += countLetters(i)
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000)
}
