package p0014

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		limit int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "under 1_000_000", args: args{1_000_000}, want: "837799"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.limit); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
