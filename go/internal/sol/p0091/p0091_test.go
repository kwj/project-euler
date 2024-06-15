package p0091

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		xSize int
		ySize int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "2x2", args: args{2, 2}, want: "14"},
		{name: "50x50", args: args{50, 50}, want: "14234"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.xSize, tt.args.ySize); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
