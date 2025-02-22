package p0098

import (
	_ "embed"
	"fmt"
	"maps"
	"slices"
	"strconv"
	"strings"

	"pe-solver/internal/mylib"
)

//go:embed 0098_words.txt
var fileContent string

type squareMap map[int][]string

func (kv *squareMap) getSquares(ndigits int) []string {
	if result, ok := (*kv)[ndigits]; ok {
		return result
	}

	lst := make([]string, 0)
	for i := mylib.Isqrt(mylib.Pow(10, ndigits-1)-1) + 1; i <= mylib.Isqrt(mylib.Pow(10, ndigits)-1); i++ {
		lst = append(lst, fmt.Sprintf("%d", i*i))
	}
	(*kv)[ndigits] = lst

	return lst
}

func makeMappingFn(m map[rune]rune) func(rune) rune {
	fn := func(r rune) rune {
		if v, ok := m[r]; ok {
			return v
		}
		return r
	}

	return fn
}

func checkAnagram(words, squares []string) int {
	transTbl := func(s1, s2 string) map[rune]rune {
		t1 := make(map[rune]rune)
		for i, char := range s1 {
			t1[char] = rune(s2[i])
		}
		t2 := make(map[rune]rune)
		for k, v := range maps.All(t1) {
			t2[v] = k
		}

		return t2
	}

	aux := func(w1, w2 string) int {
		var result int

		for sq := range slices.Values(squares) {
			mappingFn := makeMappingFn(transTbl(sq, w1))
			if strings.Map(mappingFn, w1) != sq {
				continue
			}

			tmp := strings.Map(mappingFn, w2)
			if tmp[0] != '0' && slices.Contains(squares, tmp) {
				if x, err := strconv.Atoi(tmp); err == nil {
					result = max(result, x)
				}
				if x, err := strconv.Atoi(sq); err == nil {
					result = max(result, x)
				}
			}
		}

		return result
	}

	var result int
	for idx, w1 := range slices.All(words) {
		for w2 := range slices.Values(words[idx+1:]) {
			result = max(result, aux(w1, w2))
		}
	}

	return result
}

func compute(data string) string {
	wordTbl := make(map[string][]string)
	for word := range strings.SplitSeq(strings.Trim(data, `"`), `","`) {
		tmp := []rune(word)
		slices.Sort(tmp)
		key := string(tmp)

		if v, ok := wordTbl[key]; ok {
			wordTbl[key] = append(v, word)
		} else {
			wordTbl[key] = []string{word}
		}
	}
	sqTbl := squareMap{}

	var result int
	for key, words := range maps.All(wordTbl) {
		if len(words) < 2 {
			continue
		}
		result = max(result, checkAnagram(words, sqTbl.getSquares(len(key))))
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(fileContent)
}
