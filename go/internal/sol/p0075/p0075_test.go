package p0075

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		length int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "L <= 48", args: args{48}, want: "6"},
		{name: "L <= 1500000", args: args{1_500_000}, want: "161667"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.length); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
