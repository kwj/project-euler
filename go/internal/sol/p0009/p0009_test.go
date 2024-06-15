package p0009

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		perim int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "perimeter 12 (3 + 4 + 5)", args: args{12}, want: "60"},
		{name: "perimeter 36 (9 + 12 + 15)", args: args{36}, want: "1620"},
		{name: "perimeter 1_000)", args: args{1_000}, want: "31875000"}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.perim); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
