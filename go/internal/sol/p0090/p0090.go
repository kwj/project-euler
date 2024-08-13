package p0090

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

type square struct {
	d1 int
	d2 int
}

// use '6' instead of '9'.
var squares = []square{
	{0, 1}, // 1^2
	{0, 4}, // 2^2
	{0, 6}, // 3^2
	{1, 6}, // 4^2
	{2, 5}, // 5^2
	{3, 6}, // 6^2
	{4, 6}, // 7^2, 8^2
	{8, 1}, // 9^2
}

// for easier reading
type die = []int

func isContain(twoDice []die, sq square) bool {
	if slices.Contains(twoDice[0], sq.d1) && slices.Contains(twoDice[1], sq.d2) {
		return true
	}
	if slices.Contains(twoDice[1], sq.d1) && slices.Contains(twoDice[0], sq.d2) {
		return true
	}

	return false
}

func checkSquare(twoDice []die) bool {
	if len(twoDice) != 2 {
		panic("there are not two dice")
	}

	for sq := range slices.Values(squares) {
		if !isContain(twoDice, sq) {
			return false
		}
	}

	return true
}

func compute() string {
	// use '6' instead of '9'.
	var dice []die
	dice = slices.Collect(mylib.Combinations([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 6}, 6))

	var result int
	for twoDice := range mylib.CombinationsWithRepetition(dice, 2) {
		if checkSquare(twoDice) {
			result++
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute()
}
