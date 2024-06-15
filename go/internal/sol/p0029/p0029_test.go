package p0029

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		upper int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "range: [2, 5]", args: args{5}, want: "15"},
		{name: "range: [2, 100]", args: args{100}, want: "9183"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.upper); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
