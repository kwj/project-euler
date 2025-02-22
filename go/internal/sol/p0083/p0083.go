package p0083

/*
Dijkstra's algorithm
*/

import (
	"container/heap"
	_ "embed"
	"slices"
	"strconv"
	"strings"
)

//go:embed 0083_matrix.txt
var fileContent string

type pair struct {
	r int
	c int
}

func (p pair) values() (int, int) {
	return p.r, p.c
}

func parseData(data string) [][]int {
	result := make([][]int, 0)

	for line := range strings.SplitSeq(strings.Trim(data, "\n"), "\n") {
		tmp := make([]int, 0)
		for s := range strings.SplitSeq(line, ",") {
			if n, err := strconv.Atoi(s); err == nil {
				tmp = append(tmp, n)
			}
		}
		result = append(result, tmp)
	}

	return result
}

func neighborTbl(nRow, nCol int) [][][]pair {
	filterFn := func(p pair) bool {
		if p.r < 0 || p.r >= nRow {
			return true
		}
		if p.c < 0 || p.c >= nCol {
			return true
		}

		return false
	}

	tbl := make([][][]pair, nRow)
	for i := range nRow {
		tbl[i] = make([][]pair, nCol)
	}
	for r := range nRow {
		for c := range nCol {
			tbl[r][c] = slices.DeleteFunc([]pair{{r - 1, c}, {r + 1, c}, {r, c - 1}, {r, c + 1}}, filterFn)
		}
	}

	return tbl
}

func distanceTbl(nRow, nCol int) [][]int {
	const maxInt = int(^uint(0) >> 1) // sentinel
	tbl := make([][]int, nRow)
	for i := range nRow {
		tbl[i] = make([]int, nCol)
	}
	for r := range nRow {
		for c := range nCol {
			tbl[r][c] = maxInt
		}
	}

	return tbl
}

func compute(data string) string {
	matrix := parseData(data)
	nbrTbl := neighborTbl(len(matrix), len(matrix[0]))
	distTbl := distanceTbl(len(matrix), len(matrix[0]))
	distTbl[0][0] = matrix[0][0]

	// 'priorityQueue' and 'item' are defined in the pqueue_intf.go file.
	pq := make(priorityQueue, 1)
	pq[0] = &item{node: pair{0, 0}, priority: distTbl[0][0], index: 0}
	heap.Init(&pq)

	for pq.Len() > 0 {
		pairItem := heap.Pop(&pq).(*item)
		i, j := pairItem.node.values()
		d := pairItem.priority
		for p := range slices.Values(nbrTbl[i][j]) {
			x, y := p.values()
			if newDistance := d + matrix[x][y]; newDistance < distTbl[x][y] {
				distTbl[x][y] = newDistance
				heap.Push(&pq, &item{node: pair{x, y}, priority: newDistance})
			}
		}
	}

	return strconv.FormatInt(int64(distTbl[len(distTbl)-1][len(distTbl[0])-1]), 10)
}

func Solve() string {
	return compute(fileContent)
}
