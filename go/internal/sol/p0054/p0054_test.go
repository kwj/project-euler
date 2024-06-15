package p0054

import (
	_ "embed"
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
		// The variable 'fileContent' is defined in p0054.go
		{name: "0054_poker.txt", args: args{fileContent}, want: "376"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.data); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
