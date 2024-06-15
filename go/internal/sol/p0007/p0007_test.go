package p0007

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		nth uint
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "6th prime", args: args{6}, want: "13"},
		{name: "10001th prime", args: args{10_001}, want: "104743"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.nth); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
