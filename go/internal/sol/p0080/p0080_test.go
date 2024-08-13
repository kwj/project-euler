package p0080

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		limit int
		digit int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "up to 2", args: args{2, 100}, want: "475"},
		{name: "up to 100", args: args{100, 100}, want: "40886"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.limit, tt.args.digit); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
