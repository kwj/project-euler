package p0028

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		length int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "length 5", args: args{5}, want: "101"},
		{name: "length 1001", args: args{1_001}, want: "669171001"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.length); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
