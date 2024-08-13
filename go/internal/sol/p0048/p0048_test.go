package p0048

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		upper int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "upper 10", args: args{10}, want: "0405071317"},
		{name: "upper 1000", args: args{1_000}, want: "9110846700"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.upper); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
