package p0045

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		nth int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "1st number", args: args{1}, want: "1"},
		{name: "2nd number", args: args{2}, want: "40755"},
		{name: "3rd number", args: args{3}, want: "1533776805"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.nth); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
