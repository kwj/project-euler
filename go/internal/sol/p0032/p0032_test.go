package p0032

import "testing"

func Test_compute(t *testing.T) {
	tests := []struct {
		name string
		want string
	}{
		{name: "9-digits", want: "45228"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
