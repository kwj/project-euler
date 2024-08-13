package p0010

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
		{name: "below 10", args: args{10}, want: "17"},
		{name: "below 2_000_000", args: args{2_000_000}, want: "142913828922"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.upper); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
