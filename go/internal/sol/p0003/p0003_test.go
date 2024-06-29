package p0003

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		n int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "number 13195", args: args{13_195}, want: "29"},
		{name: "number 600851475143", args: args{600_851_475_143}, want: "6857"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.n); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
