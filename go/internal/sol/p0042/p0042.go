package p0042

import (
	_ "embed"
	"strconv"
	"strings"

	"pe-solver/internal/mylib"
)

//go:embed 0042_words.txt
var fileContent string

func calcScores(words []string) []int {
	tbl := map[rune]int{}
	value := 1
	for ch := 'A'; ch <= 'Z'; ch++ {
		tbl[ch] = value
		value++
	}

	score := func(word string) int {
		var result int
		for _, ch := range word {
			result += tbl[ch]
		}

		return result
	}

	result := make([]int, len(words))
	for idx, word := range words {
		result[idx] = score(word)
	}

	return result
}

func compute(data string) string {
	words := strings.Split(strings.Trim(data, `"`), `","`)
	var result int
	for _, x := range calcScores(words) {
		if mylib.IsTriangular(x) {
			result++
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(fileContent)
}
