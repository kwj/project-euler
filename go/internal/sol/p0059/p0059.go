package p0059

import (
	_ "embed"
	"slices"
	"strconv"
	"strings"

	"pe-solver/internal/mylib"
)

//go:embed 0059_cipher.txt
var fileContent string

/*
0x20 (space): 3
0x41 - 0x5A (uppercase letters, 'A' - 'Z'): 5
0x61 - 0x7A (lowercase letters, 'a' - 'z'): 3
0x21 - 0x7E (printable characters except alphabet letters): 1
others: 0
*/
func evalChar(c byte) int {
	switch {
	case c == 0x20:
		return 3
	case 0x41 <= c && c <= 0x5a:
		return 5
	case 0x61 <= c && c <= 0x7a:
		return 3
	case 0x21 <= c && c <= 0x7e:
		return 1
	default:
		return 0
	}
}

func compute(data string) string {
	sum := func(xs []byte) int {
		var result int
		for v := range slices.Values(xs) {
			result += int(v)
		}

		return result
	}

	// 0x61: 'a', ..., 0x7A: 'z'
	keyRange := make([]byte, 0)
	for x := byte(0x61); x <= byte(0x7A); x++ {
		keyRange = append(keyRange, x)
	}

	encryptedData := make([]byte, 0)
	for s := range slices.Values(strings.Split(data, ",")) {
		if n, err := strconv.Atoi(s); err == nil {
			encryptedData = append(encryptedData, byte(n))
		} else {
			panic("invalid data")
		}
	}
	plainData := make([]byte, len(encryptedData))

	var maxScore int
	var result int
	for secretKey := range mylib.CartesianProduct(keyRange, keyRange, keyRange) {
		score := 0
		for idx, x := range slices.All(encryptedData) {
			plainData[idx] = x ^ secretKey[idx%3]
			score += evalChar(plainData[idx])
		}
		if score > maxScore {
			maxScore = score
			result = sum(plainData)
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(fileContent)
}
