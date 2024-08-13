package p0022

import (
	_ "embed"
	"slices"
	"strconv"
	"strings"
)

//go:embed 0022_names.txt
var fileContent string

func totalScore(words []string) int {
	score := func(s string) int {
		var result int32
		for _, char := range s {
			result += int32(char) - int32('A') + 1
		}

		return int(result)
	}

	var result int
	for i, word := range slices.All(words) {
		result += (i + 1) * score(word)
	}

	return result
}

func compute(data string) string {
	words := strings.Split(strings.Trim(data, `"`), `","`)
	slices.Sort(words)

	return strconv.FormatInt(int64(totalScore(words)), 10)
}

func Solve() string {
	return compute(fileContent)
}
