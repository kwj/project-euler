package p0016

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		b int
		e int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "exp: 15", args: args{2, 15}, want: "26"},
		{name: "exp: 1000", args: args{2, 1_000}, want: "1366"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.b, tt.args.e); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
