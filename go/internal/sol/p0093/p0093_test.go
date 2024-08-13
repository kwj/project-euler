package p0093

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	tests := []struct {
		name string
		want string
	}{
		{name: "Arithmetic expressions", want: "1258"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
