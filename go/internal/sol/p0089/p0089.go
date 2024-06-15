package p0089

import (
	_ "embed"
	"regexp"
	"strconv"
	"strings"
)

//go:embed 0089_roman.txt
var fileContent string

func replaceRomanNumbers(line string) string {
	var s string

	// step 1a: IX/XC/CM
	s = regexp.MustCompile(`IIIIIIIII|XXXXXXXXX|CCCCCCCCC`).ReplaceAllString(line, "##")
	// step 1b: IX/XC/CM
	s = regexp.MustCompile(`VIIII|LXXXX|DCCCC`).ReplaceAllString(s, "##")
	// step 2: V/L/D
	s = regexp.MustCompile(`IIIII|XXXXX|CCCCC`).ReplaceAllString(s, "#")
	// step 3: IV/XL/CD
	s = regexp.MustCompile(`IIII|XXXX|CCCC`).ReplaceAllString(s, "##")

	return s
}

func compute(data string) string {
	var result int
	for _, line := range strings.Split(strings.Trim(data, "\n"), "\n") {
		result += len(line) - len(replaceRomanNumbers(line))
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(fileContent)
}
