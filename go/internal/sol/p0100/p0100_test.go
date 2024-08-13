package p0100

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		boundary int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "over 10^12", args: args{1_000_000_000_000}, want: "756872327473"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.boundary); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
