package p0031

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		numbers []int
		target  int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "2 pound", args: args{[]int{1, 2, 5, 10, 20, 50, 100, 200}, 200}, want: "73682"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.numbers, tt.args.target); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
