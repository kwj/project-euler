package p0019

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	tests := []struct {
		name string
		want string
	}{
		{name: "Jan 1, 1901 - Dec 1, 2000", want: "171"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
