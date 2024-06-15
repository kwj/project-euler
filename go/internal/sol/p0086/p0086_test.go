package p0086

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		boundary int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "over 1975", args: args{1_975}, want: "100"},
		{name: "over 1000000", args: args{1_000_000}, want: "1818"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.boundary); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
