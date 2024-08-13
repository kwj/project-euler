package p0001

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		limit uint
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "below 10", args: args{10}, want: "23"},
		{name: "below 1000", args: args{1000}, want: "233168"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.limit); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
