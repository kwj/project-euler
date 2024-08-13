package p0088

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
		{name: "2 <= k = 6", args: args{6}, want: "30"},
		{name: "2 <= k  12", args: args{12}, want: "61"},
		{name: "2 <= k  12000", args: args{12_000}, want: "7587457"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.limit); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
