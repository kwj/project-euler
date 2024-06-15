package p0098

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
		// The variable 'fileContent' is defined in p0098.go
		{name: "0098_words.txt", args: args{fileContent}, want: "18769"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.data); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
