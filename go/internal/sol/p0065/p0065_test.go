package p0065

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
		{name: "10th", args: args{10}, want: "17"},
		{name: "100th", args: args{100}, want: "272"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.nth); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
