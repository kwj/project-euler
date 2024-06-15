package p0012

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		thr int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "threshold: 5", args: args{5}, want: "28"},
		{name: "threshold: 500", args: args{500}, want: "76576500"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.thr); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
