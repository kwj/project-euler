package p0096

/*
      C0 C1 C2 C3 C4 C5 C6 C7 C8
     +--------+--------+--------+    R: Row
  R0 | 0  1  2| 3  4  5| 6  7  8|    C: Column
  R1 | 9 10 11|12      |15      |
  R2 |18      |21      |24      |    Cell: 0..=80
     +--------+--------+--------+
  R3 |27      |30      |33      |
  R4 |36      |39      |42      |
  R5 |45      |48      |51      |
     +--------+--------+--------+
  R6 |54      |57      |60      |
  R7 |63      |66      |69      |
  R8 |72      |75 76 77|78 79 80|
     +--------+--------+--------+

  grid: [u128; 10]
    grid[n] is the information of number 'n'

    127  81 80                   0
    |......|.....................|
     unused       bit table

  Example:
   * Grid 01 (from p096_sudoku.txt)
      003|020|600
      900|305|001
      001|806|400
      ---+---+---
      008|102|900
      700|000|008
      006|708|200
      ---+---+---
      002|609|500
      800|203|009
      005|010|300

    Initial Data: grid[]
      [0x1ab6b64f26fec9e4dadab,
       0x010000000000040120000, 0x000040108000100000010, 0x040100000000000001004,
       0x000000000000001000000, 0x004001000000000004000, 0x000000200800000800040,
       0x000000001001000000000, 0x000008004100020200000, 0x000800800000200000200]

    1) number '0'
        python> bin(grid[0])[::-1][0:10]    # print only 'R0'
        '110101011'

        R0: 003|020|600
           '110 101 011'

    2) number '3'
        python> bin(grid[3])[::-1][0:10]    # print only 'R0'
        '001000000'

        R0: 003|020|600
           '001 000 000'

   * Grid 01 (Solution)
      483|921|657
      967|345|821
      251|876|493
      ---+---+---
      548|132|976
      729|564|138
      136|798|245
      ---+---+---
      372|689|514
      814|253|769
      695|417|382

    Solution: sol[]
      [0x1ab6b64f26fec9e4dadab,
       0x010012000240040120020, 0x100040108002100050010, 0x040100040480084001004,
       0x008024010020011002001, 0x004081020008008084080, 0x001400200810800800440,
       0x020200081001400400900, 0x080008404100020208002, 0x002800802004202000208]

    1) number '4'
      python> "{:081b}".format(sol[4])[::-1]
     '100000000000010000000000100010000000000001000000000010000000001001000000000100000'
      python> s = "{:081b}".format(sol[4])[::-1]
      python> for i in range(0, len(s), 9):
                  print(s[i:i+9])

      100000000
      000010000
      000000100
      010000000
      000001000
      000000010
      000000001
      001000000
      000100000

        The set bits indicate that there is a number at position. In this case, the number is 4.
        Please compare it with the solution of Grid 01.
*/

import (
	_ "embed"
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

//go:embed 0096_sudoku.txt
var fileContent string

const NUM_OF_CELLS = 81

// Sigh, I wish I could get the uint128 data type.
type bitTable struct {
	h uint64
	l uint64
}

func (p bitTable) notZero() bool {
	return p.h != 0 || p.l != 0
}

func (p bitTable) leftShift(n int) bitTable {
	switch {
	case n > 127:
		p.h = 0
		p.l = 0
	case n > 63:
		p.h = (p.l << (n - 64))
		p.l = 0
	default:
		p.h <<= n
		p.h |= (p.l >> (64 - n))
		p.l <<= n
	}

	return p
}

func (p bitTable) and(mask bitTable) bitTable {
	tmp := bitTable{h: 0, l: 0}
	tmp.h = p.h & mask.h
	tmp.l = p.l & mask.l

	return tmp
}

func (p bitTable) or(mask bitTable) bitTable {
	tmp := bitTable{h: 0, l: 0}
	tmp.h = p.h | mask.h
	tmp.l = p.l | mask.l

	return tmp
}

func (p bitTable) not() bitTable {
	p.h = ^p.h
	p.l = ^p.l

	return p
}

/*
_ How to create 'adjacentCells' _

// 0b: 111111111
var rowMask = bitTable{h: 0, l: 0x1ff}

// 0b: 000000001 000000001 ... 000000001
var colMask = bitTable{h: 0x100, l: 0x8040201008040201}

// 0b: 000000111 000000111 000000111
var boxMask = bitTable{h: 0, l: 0x1c0e07}

var adjacentCells [NUM_OF_CELLS]bitTable

func initAdjacentCells() {
	for pos := range NUM_OF_CELLS {
		rowShift := (pos / 9) * 9
		colShift := pos % 9
		boxShift := (pos/27)*27 + (colShift/3)*3

		adjacentCells[pos] = adjacentCells[pos].or(rowMask.leftShift(rowShift))
		adjacentCells[pos] = adjacentCells[pos].or(colMask.leftShift(colShift))
		adjacentCells[pos] = adjacentCells[pos].or(boxMask.leftShift(boxShift))
	}
}
*/

var adjacentCells = [NUM_OF_CELLS]bitTable{
	{h: 0x100, l: 0x80402010081c0fff}, {h: 0x201, l: 0x804020101c0fff},
	{h: 0x402, l: 0x1008040201c0fff}, {h: 0x804, l: 0x201008040e071ff},
	{h: 0x1008, l: 0x402010080e071ff}, {h: 0x2010, l: 0x804020100e071ff},
	{h: 0x4020, l: 0x10080402070381ff}, {h: 0x8040, l: 0x20100804070381ff},
	{h: 0x10080, l: 0x40201008070381ff}, {h: 0x100, l: 0x80402010081ffe07},
	{h: 0x201, l: 0x804020101ffe07}, {h: 0x402, l: 0x1008040201ffe07},
	{h: 0x804, l: 0x201008040e3fe38}, {h: 0x1008, l: 0x402010080e3fe38},
	{h: 0x2010, l: 0x804020100e3fe38}, {h: 0x4020, l: 0x100804020703ffc0},
	{h: 0x8040, l: 0x201008040703ffc0}, {h: 0x10080, l: 0x402010080703ffc0},
	{h: 0x100, l: 0x804020100ffc0e07}, {h: 0x201, l: 0x80402017fc0e07},
	{h: 0x402, l: 0x100804027fc0e07}, {h: 0x804, l: 0x201008047fc7038},
	{h: 0x1008, l: 0x402010087fc7038}, {h: 0x2010, l: 0x804020107fc7038},
	{h: 0x4020, l: 0x1008040207ff81c0}, {h: 0x8040, l: 0x2010080407ff81c0},
	{h: 0x10080, l: 0x4020100807ff81c0}, {h: 0x100, l: 0x8040e07ff8040201},
	{h: 0x201, l: 0x80e07ff8080402}, {h: 0x402, l: 0x100e07ff8100804},
	{h: 0x804, l: 0x207038ff8201008}, {h: 0x1008, l: 0x407038ff8402010},
	{h: 0x2010, l: 0x807038ff8804020}, {h: 0x4020, l: 0x10381c0ff9008040},
	{h: 0x8040, l: 0x20381c0ffa010080}, {h: 0x10080, l: 0x40381c0ffc020100},
	{h: 0x100, l: 0x8040fff038040201}, {h: 0x201, l: 0x80fff038080402},
	{h: 0x402, l: 0x100fff038100804}, {h: 0x804, l: 0x2071ff1c0201008},
	{h: 0x1008, l: 0x4071ff1c0402010}, {h: 0x2010, l: 0x8071ff1c0804020},
	{h: 0x4020, l: 0x10381ffe01008040}, {h: 0x8040, l: 0x20381ffe02010080},
	{h: 0x10080, l: 0x40381ffe04020100}, {h: 0x100, l: 0x807fe07038040201},
	{h: 0x201, l: 0xbfe07038080402}, {h: 0x402, l: 0x13fe07038100804},
	{h: 0x804, l: 0x23fe381c0201008}, {h: 0x1008, l: 0x43fe381c0402010},
	{h: 0x2010, l: 0x83fe381c0804020}, {h: 0x4020, l: 0x103ffc0e01008040},
	{h: 0x8040, l: 0x203ffc0e02010080}, {h: 0x10080, l: 0x403ffc0e04020100},
	{h: 0x703, l: 0xffc0201008040201}, {h: 0x703, l: 0xffc0402010080402},
	{h: 0x703, l: 0xffc0804020100804}, {h: 0x381c, l: 0x7fc1008040201008},
	{h: 0x381c, l: 0x7fc2010080402010}, {h: 0x381c, l: 0x7fc4020100804020},
	{h: 0x1c0e0, l: 0x7fc8040201008040}, {h: 0x1c0e0, l: 0x7fd0080402010080},
	{h: 0x1c0e0, l: 0x7fe0100804020100}, {h: 0x7ff, l: 0x81c0201008040201},
	{h: 0x7ff, l: 0x81c0402010080402}, {h: 0x7ff, l: 0x81c0804020100804},
	{h: 0x38ff, l: 0x8e01008040201008}, {h: 0x38ff, l: 0x8e02010080402010},
	{h: 0x38ff, l: 0x8e04020100804020}, {h: 0x1c0ff, l: 0xf008040201008040},
	{h: 0x1c0ff, l: 0xf010080402010080}, {h: 0x1c0ff, l: 0xf020100804020100},
	{h: 0x1ff03, l: 0x81c0201008040201}, {h: 0x1ff03, l: 0x81c0402010080402},
	{h: 0x1ff03, l: 0x81c0804020100804}, {h: 0x1ff1c, l: 0xe01008040201008},
	{h: 0x1ff1c, l: 0xe02010080402010}, {h: 0x1ff1c, l: 0xe04020100804020},
	{h: 0x1ffe0, l: 0x7008040201008040}, {h: 0x1ffe0, l: 0x7010080402010080},
	{h: 0x1ffe0, l: 0x7020100804020100},
}

func parseData(data string) [][]int {
	convert := func(lst []string) []int {
		var s string
		for _, x := range lst {
			s += x
		}
		s = regexp.MustCompile(`[^0-9.]`).ReplaceAllLiteralString(s, "")
		s = regexp.MustCompile(`\.`).ReplaceAllLiteralString(s, "0")

		result := make([]int, 0)
		for _, x := range s {
			result = append(result, int(x)-int('0'))
		}

		return result
	}

	result := make([][]int, 0)
	acc := make([]string, 0)

	validChar := regexp.MustCompile(`^[0-9.]`)
	sepChar := regexp.MustCompile(`^-`)
	for _, line := range strings.Split(strings.Trim(data, "\n"), "\n") {
		if matched := validChar.MatchString(line); matched {
			acc = append(acc, line)
			continue
		}
		if matched := sepChar.MatchString(line); matched || len(acc) == 0 {
			continue
		}
		result = append(result, convert(acc))
		acc = make([]string, 0)
	}
	if len(acc) == 9 {
		result = append(result, convert(acc))
	}

	for _, s := range result {
		if len(s) != 81 {
			panic("invalid data")
		}
	}

	return result
}

func makeGrids(puzzles [][]int) [][]bitTable {
	result := make([][]bitTable, 0)

loop:
	for i, lst := range puzzles {
		if len(lst) != NUM_OF_CELLS {
			fmt.Printf("[Warning] size of input data is mismatch [Grid: %d] (ignored)\n%v\n", i+1, lst)
			continue
		}

		grid := make([]bitTable, 10)
		for pos, n := range lst {
			if n != 0 && grid[n].and(adjacentCells[pos]).notZero() {
				fmt.Printf("[Warning] there is a same number in the adjacent cells [Grid: %d] (ignored)\n%v\n", i+1, lst)
				continue loop
			}
			grid[n] = grid[n].or(bitTable{h: 0, l: 1}.leftShift(pos))
		}
		result = append(result, grid)
	}

	return result
}

func sudokuSolver(grid *[]bitTable) bool {
	if _sudokouSolver(grid, 0, bitTable{h: 0, l: 1}) {
		return true
	} else {
		return false
	}
}

func _sudokouSolver(grid *[]bitTable, pos int, posBit bitTable) bool {
	for {
		if pos >= NUM_OF_CELLS {
			return true
		}

		if (*grid)[0].and(posBit).notZero() {
			break
		}
		posBit = posBit.leftShift(1)
		pos++
	}

	for n := 1; n <= 9; n++ {
		if (*grid)[n].and(adjacentCells[pos]).notZero() {
			continue
		}

		(*grid)[n] = (*grid)[n].or(posBit)
		if _sudokouSolver(grid, pos+1, posBit.leftShift(1)) {
			return true
		}
		(*grid)[n] = (*grid)[n].and(posBit.not())
	}

	return false
}

func compute(data string) string {
	var result int

	for _, grid := range makeGrids(parseData(data)) {
		if sudokuSolver(&grid) {
			var tmp int
			for i := 0; i <= 2; i++ {
				bit := bitTable{h: 0, l: 1}.leftShift(i)
				for pos, elm := range grid[1:] {
					if elm.and(bit).notZero() {
						tmp = tmp*10 + (pos + 1)
						break
					}
				}
			}
			result += tmp
		} else {
			panic("every puzzle in this problem has each one unique answer")
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(fileContent)
}
