package p0079

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
		// The variable 'fileContent' is defined in p0079.go
		{name: "0079_keylog.txt", args: args{fileContent}, want: "73162890"},
	}
	for tt := range slices.Values(tests) {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.data); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
