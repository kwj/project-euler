package p0061

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		maxPolygon int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "Triangular and Square numbers", args: args{4}, want: "8181"},
		{name: "Triangular, Square and Pentagonal numbers", args: args{5}, want: "19291"},
		{name: "Triangular, Square, ... and Octagonal numbers", args: args{8}, want: "28684"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.maxPolygon); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
