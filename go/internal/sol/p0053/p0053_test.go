package p0053

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		num      int
		boundary int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "n: 23, boundary: 1000000", args: args{23, 1_000_000}, want: "4"},
		{name: "n: 100, boundary: 1000000", args: args{100, 1_000_000}, want: "4075"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.num, tt.args.boundary); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
