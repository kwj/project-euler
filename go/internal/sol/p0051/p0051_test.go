package p0051

import "testing"

func Test_compute(t *testing.T) {
	type args struct {
		familySize int
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{name: "Prime digit replacements", args: args{8}, want: "121313"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := compute(tt.args.familySize); got != tt.want {
				t.Errorf("compute() = %v, want %v", got, tt.want)
			}
		})
	}
}
