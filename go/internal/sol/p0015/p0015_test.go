package p0015

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		r int
		c int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "2x2 grid", args: args{2, 2}, want: "6"},
		{name: "20x20 grid", args: args{20, 20}, want: "137846528820"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.r, tt.args.c); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
