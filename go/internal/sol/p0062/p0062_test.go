package p0062

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		size int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "group size: 3", args: args{3}, want: "41063625"},
		{name: "group size: 5", args: args{5}, want: "127035954683"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.size); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
