package p0099

import (
	_ "embed"
	"math"
	"slices"
	"strconv"
	"strings"
)

//go:embed 0099_base_exp.txt
var fileContent string

func compute(data string) string {
	var maxValue float64
	var lineNo int

	for idx, line := range slices.All(strings.Split(strings.Trim(data, "\n"), "\n")) {
		x := strings.Split(line, ",")
		var base int
		var exp int
		base, _ = strconv.Atoi(x[0])
		exp, _ = strconv.Atoi(x[1])

		tmp := float64(exp) * math.Log10(float64(base))
		if tmp > maxValue {
			maxValue = tmp
			lineNo = idx + 1
		}
	}

	return strconv.FormatInt(int64(lineNo), 10)
}

func Solve() string {
	return compute(fileContent)
}
