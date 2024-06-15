package p0076

import "testing"

func Test_compute(t *testing.T) {
	data1 := []int{1, 2, 3, 4}
	data2 := make([]int, 99)
	for i := range data2 {
		data2[i] = i + 1
	}

	type args struct {
		numbers []int
		target  int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "total 5", args: args{data1, 5}, want: "6"},
		{name: "total 100", args: args{data2, 100}, want: "190569291"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.numbers, tt.args.target); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
