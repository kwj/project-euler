package p0060

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		groupSize int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "group size: 4", args: args{4}, want: "792"},
		{name: "group size: 5", args: args{5}, want: "26033"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.groupSize); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
