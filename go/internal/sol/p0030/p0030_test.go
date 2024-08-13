package p0030

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		e int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "fourth powers", args: args{4}, want: "19316"},
		{name: "fifth powers", args: args{5}, want: "443839"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.e); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
