package p0050

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		limit int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "below 100", args: args{100}, want: "41"},
		{name: "below 500", args: args{500}, want: "499"},
		{name: "below 1000", args: args{1_000}, want: "953"},
		{name: "below 10000", args: args{10_000}, want: "9521"},
		{name: "below 1000000", args: args{1_000_000}, want: "997651"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.limit); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
