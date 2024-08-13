package p0004

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		ndigits int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "2-digit numbers", args: args{2}, want: "9009"},
		{name: "3-digit numbers", args: args{3}, want: "906609"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.ndigits); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
