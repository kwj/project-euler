package p0051

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		familySize int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "Prime digit replacements", args: args{8}, want: "121313"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.familySize); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
