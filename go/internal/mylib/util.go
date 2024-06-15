package mylib

func Frequencies[T comparable](lst []T) map[T]int {
	result := map[T]int{}
	for _, x := range lst {
		result[x] += 1
	}

	return result
}

func FindAll[S ~[]E, E any](s S, f func(E) bool) []int {
	result := make([]int, 0)
	for idx, x := range s {
		if f(x) {
			result = append(result, idx)
		}
	}

	return result
}
