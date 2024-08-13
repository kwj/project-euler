package p0078

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		denom int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "divisible by 7", args: args{7}, want: "5"},
		{name: "divisible by one million", args: args{1_000_000}, want: "55374"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.denom); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
