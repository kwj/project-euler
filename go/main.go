package main

import (
	"fmt"
	"os"
	"time"

	"pe-solver/internal/sol/p0001"
	"pe-solver/internal/sol/p0002"
	"pe-solver/internal/sol/p0003"
	"pe-solver/internal/sol/p0004"
	"pe-solver/internal/sol/p0005"
	"pe-solver/internal/sol/p0006"
	"pe-solver/internal/sol/p0007"
	"pe-solver/internal/sol/p0008"
	"pe-solver/internal/sol/p0009"
	"pe-solver/internal/sol/p0010"
	"pe-solver/internal/sol/p0011"
	"pe-solver/internal/sol/p0012"
	"pe-solver/internal/sol/p0013"
	"pe-solver/internal/sol/p0014"
	"pe-solver/internal/sol/p0015"
	"pe-solver/internal/sol/p0016"
	"pe-solver/internal/sol/p0017"
	"pe-solver/internal/sol/p0018"
	"pe-solver/internal/sol/p0019"
	"pe-solver/internal/sol/p0020"
	"pe-solver/internal/sol/p0021"
	"pe-solver/internal/sol/p0022"
	"pe-solver/internal/sol/p0023"
	"pe-solver/internal/sol/p0024"
	"pe-solver/internal/sol/p0025"
	"pe-solver/internal/sol/p0026"
	"pe-solver/internal/sol/p0027"
	"pe-solver/internal/sol/p0028"
	"pe-solver/internal/sol/p0029"
	"pe-solver/internal/sol/p0030"
	"pe-solver/internal/sol/p0031"
	"pe-solver/internal/sol/p0032"
	"pe-solver/internal/sol/p0033"
	"pe-solver/internal/sol/p0034"
	"pe-solver/internal/sol/p0035"
	"pe-solver/internal/sol/p0036"
	"pe-solver/internal/sol/p0037"
	"pe-solver/internal/sol/p0038"
	"pe-solver/internal/sol/p0039"
	"pe-solver/internal/sol/p0040"
	"pe-solver/internal/sol/p0041"
	"pe-solver/internal/sol/p0042"
	"pe-solver/internal/sol/p0043"
	"pe-solver/internal/sol/p0044"
	"pe-solver/internal/sol/p0045"
	"pe-solver/internal/sol/p0046"
	"pe-solver/internal/sol/p0047"
	"pe-solver/internal/sol/p0048"
	"pe-solver/internal/sol/p0049"
	"pe-solver/internal/sol/p0050"
	"pe-solver/internal/sol/p0051"
	"pe-solver/internal/sol/p0052"
	"pe-solver/internal/sol/p0053"
	"pe-solver/internal/sol/p0054"
	"pe-solver/internal/sol/p0055"
	"pe-solver/internal/sol/p0056"
	"pe-solver/internal/sol/p0057"
	"pe-solver/internal/sol/p0058"
	"pe-solver/internal/sol/p0059"
	"pe-solver/internal/sol/p0060"
	"pe-solver/internal/sol/p0061"
	"pe-solver/internal/sol/p0062"
	"pe-solver/internal/sol/p0063"
	"pe-solver/internal/sol/p0064"
	"pe-solver/internal/sol/p0065"
	"pe-solver/internal/sol/p0066"
	"pe-solver/internal/sol/p0067"
	"pe-solver/internal/sol/p0068"
	"pe-solver/internal/sol/p0069"
	"pe-solver/internal/sol/p0070"
	"pe-solver/internal/sol/p0071"
	"pe-solver/internal/sol/p0072"
	"pe-solver/internal/sol/p0073"
	"pe-solver/internal/sol/p0074"
	"pe-solver/internal/sol/p0075"
	"pe-solver/internal/sol/p0076"
	"pe-solver/internal/sol/p0077"
	"pe-solver/internal/sol/p0078"
	"pe-solver/internal/sol/p0079"
	"pe-solver/internal/sol/p0080"
	"pe-solver/internal/sol/p0081"
	"pe-solver/internal/sol/p0082"
	"pe-solver/internal/sol/p0083"
	"pe-solver/internal/sol/p0084"
	"pe-solver/internal/sol/p0085"
	"pe-solver/internal/sol/p0086"
	"pe-solver/internal/sol/p0087"
	"pe-solver/internal/sol/p0088"
	"pe-solver/internal/sol/p0089"
	"pe-solver/internal/sol/p0090"
	"pe-solver/internal/sol/p0091"
	"pe-solver/internal/sol/p0092"
	"pe-solver/internal/sol/p0093"
	"pe-solver/internal/sol/p0094"
	"pe-solver/internal/sol/p0095"
	"pe-solver/internal/sol/p0096"
	"pe-solver/internal/sol/p0097"
	"pe-solver/internal/sol/p0098"
	"pe-solver/internal/sol/p0099"
	"pe-solver/internal/sol/p0100"
)

var tbl = map[string]func() string{
	"1":   p0001.Solve,
	"2":   p0002.Solve,
	"3":   p0003.Solve,
	"4":   p0004.Solve,
	"5":   p0005.Solve,
	"6":   p0006.Solve,
	"7":   p0007.Solve,
	"8":   p0008.Solve,
	"9":   p0009.Solve,
	"10":  p0010.Solve,
	"11":  p0011.Solve,
	"12":  p0012.Solve,
	"13":  p0013.Solve,
	"14":  p0014.Solve,
	"15":  p0015.Solve,
	"16":  p0016.Solve,
	"17":  p0017.Solve,
	"18":  p0018.Solve,
	"19":  p0019.Solve,
	"20":  p0020.Solve,
	"21":  p0021.Solve,
	"22":  p0022.Solve,
	"23":  p0023.Solve,
	"24":  p0024.Solve,
	"25":  p0025.Solve,
	"26":  p0026.Solve,
	"27":  p0027.Solve,
	"28":  p0028.Solve,
	"29":  p0029.Solve,
	"30":  p0030.Solve,
	"31":  p0031.Solve,
	"32":  p0032.Solve,
	"33":  p0033.Solve,
	"34":  p0034.Solve,
	"35":  p0035.Solve,
	"36":  p0036.Solve,
	"37":  p0037.Solve,
	"38":  p0038.Solve,
	"39":  p0039.Solve,
	"40":  p0040.Solve,
	"41":  p0041.Solve,
	"42":  p0042.Solve,
	"43":  p0043.Solve,
	"44":  p0044.Solve,
	"45":  p0045.Solve,
	"46":  p0046.Solve,
	"47":  p0047.Solve,
	"48":  p0048.Solve,
	"49":  p0049.Solve,
	"50":  p0050.Solve,
	"51":  p0051.Solve,
	"52":  p0052.Solve,
	"53":  p0053.Solve,
	"54":  p0054.Solve,
	"55":  p0055.Solve,
	"56":  p0056.Solve,
	"57":  p0057.Solve,
	"58":  p0058.Solve,
	"59":  p0059.Solve,
	"60":  p0060.Solve,
	"61":  p0061.Solve,
	"62":  p0062.Solve,
	"63":  p0063.Solve,
	"64":  p0064.Solve,
	"65":  p0065.Solve,
	"66":  p0066.Solve,
	"67":  p0067.Solve,
	"68":  p0068.Solve,
	"69":  p0069.Solve,
	"70":  p0070.Solve,
	"71":  p0071.Solve,
	"72":  p0072.Solve,
	"73":  p0073.Solve,
	"74":  p0074.Solve,
	"75":  p0075.Solve,
	"76":  p0076.Solve,
	"77":  p0077.Solve,
	"78":  p0078.Solve,
	"79":  p0079.Solve,
	"80":  p0080.Solve,
	"81":  p0081.Solve,
	"82":  p0082.Solve,
	"83":  p0083.Solve,
	"84":  p0084.Solve,
	"85":  p0085.Solve,
	"86":  p0086.Solve,
	"87":  p0087.Solve,
	"88":  p0088.Solve,
	"89":  p0089.Solve,
	"90":  p0090.Solve,
	"91":  p0091.Solve,
	"92":  p0092.Solve,
	"93":  p0093.Solve,
	"94":  p0094.Solve,
	"95":  p0095.Solve,
	"96":  p0096.Solve,
	"97":  p0097.Solve,
	"98":  p0098.Solve,
	"99":  p0099.Solve,
	"100": p0100.Solve,
}

func runSolver(n string, fn func() string) {
	t0 := time.Now()
	result := fn()
	elapsedTime := time.Since(t0)

	fmt.Printf("[Problem %s]\n", n)
	fmt.Printf("Answer: %s\n", result)
	fmt.Printf("Elapsed time: %s\n\n", elapsedTime)
}

func main() {
	if len(os.Args) < 2 {
		println("Usage:")
		fmt.Printf("  %s <problem number ...>\n\n", os.Args[0])
		os.Exit(1)
	}

	for _, x := range os.Args[1:] {
		fn, ok := tbl[x]
		if ok {
			runSolver(x, fn)
		} else {
			fmt.Printf("No solver exists for problem '%s'.\n\n", x)
		}
	}
}
