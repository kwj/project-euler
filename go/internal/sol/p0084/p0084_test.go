package p0084

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		side      int
		nAttempts int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "4-sided dice", args: args{4, 1_000_000}, want: "101524"},
		// With 6-sided dice, the answer is not stable. I don't know why.
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.side, tt.args.nAttempts); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
