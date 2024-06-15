package p0024

import "testing"

func Test_compute(t *testing.T) {
	lst := []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

	type args struct {
		nth    int
		elmLst []int
		depth  int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "1_000_000th number", args: args{1_000_000, lst, len(lst)}, want: "2783915460"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.nth, tt.args.elmLst, tt.args.depth); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
