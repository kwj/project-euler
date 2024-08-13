package p0054

import (
	_ "embed"
	"slices"
	"strconv"
	"strings"
)

//go:embed 0054_poker.txt
var fileContent string

var straightNumbers = [][]int{
	{14, 13, 12, 11, 10}, {13, 12, 11, 10, 9}, {12, 11, 10, 9, 8},
	{11, 10, 9, 8, 7}, {10, 9, 8, 7, 6}, {9, 8, 7, 6, 5},
	{8, 7, 6, 5, 4}, {7, 6, 5, 4, 3}, {6, 5, 4, 3, 2},
}

/*
0: High Card - [1 1 1 1 1]
1: One Pair [2 1 1 1]
2: Two Pairs [2 2 1]
3: Three of a Kind [3 1 1]
4: Straight []  (Note: dummy pattern for indexOf())
5: Flush []  (Note: dummy pattern for indexOf())
6: Full House [3 2]
7: Four of a Kind [4 1]

Note:
This data can't be used for Straight/Flush/Straight Flush/Royal Flush.
*/
var handPatterns = [][]int{
	{1, 1, 1, 1, 1}, {2, 1, 1, 1}, {2, 2, 1}, {3, 1, 1},
	{}, {}, {3, 2}, {4, 1},
}

func cardNumber(s string) int {
	tbl := map[byte]int{
		'2': 2, '3': 3, '4': 4, '5': 5, '6': 6,
		'7': 7, '8': 8, '9': 9, 'T': 10, 'J': 11,
		'Q': 12, 'K': 13, 'A': 14,
	}

	return tbl[s[0]]
}

func isFlush[T comparable](m map[T]struct{}) bool {
	return len(m) == 1
}

// If 'xs' is an element of 'xss', return its position (>=0) in 'xss'.
// Otherwise, return -1.
func indexOf(xss [][]int, xs []int) int {
	for idx, lst := range slices.All(xss) {
		if slices.Compare(lst, xs) == 0 {
			return idx
		}
	}

	return -1
}

func isStraight(nums []int) bool {
	return indexOf(straightNumbers, nums) >= 0
}

/*
Return hand rank and hand numbers

example: JH 2D JS QD AC -> 1, [11 14 12 2]

hand rank:
0: High Card
1: One Pair
2: Two Pairs
3: Three of a Kind
4: Straight
5: Flush
6: Full House
7: Four of a Kind
8: Straight Flush
9: Royal Flush
*/
func getHand(cards []string) (int, []int) {
	nums := make([]int, 0)
	suits := make(map[byte]struct{})
	for card := range slices.Values(cards) {
		nums = append(nums, cardNumber(card))
		suits[card[1]] = struct{}{}
	}
	slices.Sort(nums)
	slices.Reverse(nums)

	numsGrp := make([][]int, 0)
	tmp := make([]int, 0)
	for v := range slices.Values(nums) {
		if len(tmp) != 0 && tmp[0] != v {
			numsGrp = append(numsGrp, tmp)
			tmp = []int{v}
		} else {
			tmp = append(tmp, v)
		}
	}
	if len(tmp) != 0 {
		numsGrp = append(numsGrp, tmp)
	}

	pattern := make([]int, 0)
	for grp := range slices.Values(numsGrp) {
		pattern = append(pattern, len(grp))
	}
	slices.Sort(pattern)
	slices.Reverse(pattern)

	handNums := make([]int, 0)
	slices.SortStableFunc(numsGrp, func(a, b []int) int { return len(b) - len(a) })
	for grp := range slices.Values(numsGrp) {
		handNums = append(handNums, grp[0])
	}

	switch {
	case isFlush(suits):
		hand := indexOf(straightNumbers, nums)
		switch {
		case hand == 0:
			return 9, handNums // royal flush
		case hand > 0:
			return 8, handNums // starigh flush
		default:
			return 5, handNums // flush
		}
	case isStraight(nums):
		return 4, handNums
	default:
		return indexOf(handPatterns, pattern), handNums
	}
}

func parseData(data string) [][][]string {
	lines := strings.Split(strings.Trim(data, "\n"), "\n")
	result := make([][][]string, 0)
	for line := range slices.Values(lines) {
		cards := strings.Split(line, " ")
		if len(cards) != 10 {
			panic("invalid data")
		}
		result = append(result, [][]string{cards[0:5], cards[5:]})
	}

	return result
}

func judge(game [][]string) int {
	p1Hand, p1HandNums := getHand(game[0])
	p2Hand, p2HandNums := getHand(game[1])

	if hand := p1Hand - p2Hand; hand != 0 {
		return hand
	}

	return slices.Compare(p1HandNums, p2HandNums)
}

func compute(data string) string {
	var result int
	for game := range slices.Values(parseData(data)) {
		if judge(game) > 0 {
			result++
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(fileContent)
}
