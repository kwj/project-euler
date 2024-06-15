package p0047

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
		{name: "two consecutive numbers", args: args{2}, want: "14"},
		{name: "three consecutive numbersr", args: args{3}, want: "644"},
		{name: "four consecutive numbers", args: args{4}, want: "134043"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.length); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
