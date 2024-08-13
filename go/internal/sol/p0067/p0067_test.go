package p0067

import (
	"slices"
	"testing"
)

func Test_compute(t *testing.T) {
	type args struct {
		data string
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		// The variable 'fileContent' is defined in p0067.go
		{name: "0067_triangle.txt", args: args{fileContent}, want: "7273"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.data); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
