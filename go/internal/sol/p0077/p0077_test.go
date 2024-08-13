package p0077

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
		{name: "over 2 different ways", args: args{2}, want: "8"},
		{name: "over 4 different ways", args: args{4}, want: "10"},
		{name: "over 5000 different ways", args: args{5_000}, want: "71"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.boundary); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
