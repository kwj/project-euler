package p0068

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		nGon int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "3-gon ring", args: args{3}, want: "432621513"},
		{name: "5-gon ring", args: args{5}, want: "6531031914842725"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.nGon); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
