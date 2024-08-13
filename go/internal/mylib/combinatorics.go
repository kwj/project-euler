package mylib

import (
	"iter"
	"slices"
)

func Combinations[T any](xs []T, k int) iter.Seq[[]T] {
	if k < 0 {
		panic("'k' must be a non-negative number")
	}

	return func(yield func(v []T) bool) {
		n := len(xs)
		if n < k {
			return
		}
		indices := make([]int, n)
		for i := range len(indices) {
			indices[i] = i
		}
		if !yield(slices.Clone(xs[0:k])) {
			return
		}

		for {
			var i int
			for i = k - 1; i >= 0; i-- {
				if indices[i] != i+n-k {
					break
				}
			}
			if i < 0 {
				return
			}

			result := make([]T, k)
			for j := range i {
				result[j] = xs[indices[j]]
			}

			indices[i]++
			idx := indices[i]
			result[i] = xs[idx]
			for j := i + 1; j < k; j++ {
				idx++
				indices[j] = idx
				result[j] = xs[idx]
			}
			if !yield(result) {
				return
			}
		}
	}
}

func CombinationsWithRepetition[T any](xs []T, k int) iter.Seq[[]T] {
	if k < 0 {
		panic("'k' must be a non-negative number")
	}

	return func(yield func(v []T) bool) {
		n := len(xs)
		if n == 0 && k > 0 {
			return
		}
		indices := make([]int, n)

		tmp := make([]T, k)
		for i := range len(tmp) {
			tmp[i] = xs[0]
		}
		if !yield(tmp) {
			return
		}

		for {
			var i int
			for i = k - 1; i >= 0; i-- {
				if indices[i] != n-1 {
					break
				}
			}
			if i < 0 {
				return
			}

			result := make([]T, k)
			for j := range i {
				result[j] = xs[indices[j]]
			}

			indices[i]++
			idx := indices[i]
			elmnt := xs[idx]
			for j := i; j < k; j++ {
				indices[j] = idx
				result[j] = elmnt
			}
			if !yield(result) {
				return
			}
		}
	}
}

func Permutations[T any](xs []T, k int) iter.Seq[[]T] {
	if k < 0 {
		panic("'k' must be a non-negative number")
	}

	return func(yield func(v []T) bool) {
		n := len(xs)
		if n < k {
			return
		}
		indices := make([]int, n)
		for i := range len(indices) {
			indices[i] = i
		}
		swapPos := make([]int, k)
		for i := range len(swapPos) {
			swapPos[i] = n - i
		}
		if !yield(slices.Clone(xs[0:k])) {
			return
		}

		var i int
		for i >= 0 {
			for i = k - 1; i >= 0; i-- {
				swapPos[i]--
				if swapPos[i] != 0 {
					j := n - swapPos[i]
					indices[i], indices[j] = indices[j], indices[i]
					result := make([]T, k)
					for idx := range len(result) {
						result[idx] = xs[indices[idx]]
					}
					if !yield(result) {
						return
					}
					break
				} else {
					idx := indices[i]
					for j := n - 1; j >= i; j-- {
						idx, indices[j] = indices[j], idx
					}
					swapPos[i] = n - i
				}
			}
		}
	}
}

func PermutationsWithRepetition[T any](xs []T, k int) iter.Seq[[]T] {
	if k < 0 {
		panic("'k' must be a non-negative number")
	}

	return func(yield func(v []T) bool) {
		n := len(xs)
		if n == 0 && k > 0 {
			return
		}
		indices := make([]int, n)

		tmp := make([]T, k)
		for i := range len(tmp) {
			tmp[i] = xs[0]
		}
		if !yield(tmp) {
			return
		}

		for {
			for i := k - 1; i >= 0; i-- {
				if indices[i] == n-1 {
					continue
				}
				result := make([]T, k)
				for j := range i {
					result[j] = xs[indices[j]]
				}
				indices[i]++
				idx := indices[i]
				result[i] = xs[idx]
				for j := i + 1; j < k; j++ {
					indices[j] = 0
					result[j] = xs[0]
				}
				if !yield(tmp) {
					return
				}
				break
			}
		}
	}
}

func CartesianProduct[T any](xss ...[]T) iter.Seq[[]T] {
	return func(yield func(v []T) bool) {
		for xs := range slices.Values(xss) {
			if len(xs) == 0 {
				return
			}
		}

		n := len(xss)
		tmp := make([]T, n)
		if n == 0 {
			yield(tmp)
			return
		} else {
			for idx, xs := range slices.All(xss) {
				tmp[idx] = xs[0]
			}
			if !yield(tmp) {
				return
			}
		}

		indices := make([]int, n)

	loop:
		for {
			for i := n - 1; i >= 0; i-- {
				if indices[i] == len(xss[i])-1 {
					continue
				}
				result := make([]T, n)
				for j := range i {
					result[j] = xss[j][indices[j]]
				}
				indices[i]++
				idx := indices[i]
				result[i] = xss[i][idx]
				for j := i + 1; j < n; j++ {
					indices[j] = 0
					result[j] = xss[j][0]
				}
				if !yield(result) {
					return
				}
				continue loop
			}

			return
		}
	}
}

func PowerSet[T any](xs []T) iter.Seq[[]T] {
	if len(xs) > intSize-1 {
		panic("too many elements")
	}

	return func(yield func(v []T) bool) {
		var mask uint
		for mask = 0; mask < (1 << len(xs)); mask++ {
			result := make([]T, 0)
			for pos, x := range slices.All(xs) {
				if (mask>>pos)&1 == 1 {
					result = append(result, x)
				}
			}
			if !yield(result) {
				return
			}
		}
	}
}
