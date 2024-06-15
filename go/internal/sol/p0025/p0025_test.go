package p0025

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		ndigits int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "3-digit", args: args{3}, want: "12"},
		{name: "1000-digit", args: args{1_000}, want: "4782"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.ndigits); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
