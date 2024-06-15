package p0040

import "testing"

func Test_compute(t *testing.T) {
	tests := []struct {
		name string
		want string
	}{
		{name: "Champernowne's Constant", want: "210"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
