package p0085

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		target int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "target rectangles", args: args{2_000_000}, want: "2772"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.target); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
