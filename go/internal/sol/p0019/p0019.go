package p0019

import (
	"slices"
	"strconv"
)

func compute() string {
	commonYear := []int{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
	leapYear := []int{31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}

	// days per month (Jan 1901 - *Nov* 2000)
	days := slices.Repeat(slices.Concat(commonYear, commonYear, commonYear, leapYear), 25)
	days = days[:len(days)-1] // drop Dec 2000

	// insert number of days of the year 1900 (common year)
	days = slices.Insert(days, 0, 365)

	// accumulate
	for i := range len(days) - 1 {
		days[i+1] += days[i]
	}

	// The year 1900 was a comman year and January 1, 1900 was a Monday.
	// --> Monday: 0, Tuesday: 1, ..., Sunday: 6
	//     Jan 1, 1901 was Tuesday since 365 % 7 = 1.
	//     Feb 1, 1901 was Firday since (365 + 31) % 7 = 4.
	//     Mar 1, 1901 was Firday since (365 + 31 + 28) % 7 = 4.
	//     ... and so on
	var cnt int
	for x := range slices.Values(days) {
		// If it is Sunday, count it
		if x%7 == 6 {
			cnt++
		}
	}

	return strconv.FormatInt(int64(cnt), 10)
}

func Solve() string {
	return compute()
}
