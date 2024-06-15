package p0084

import (
	"cmp"
	"fmt"
	"math/rand/v2"
	"slices"
)

const (
	// squares
	GO = iota
	A1
	CC1
	A2
	T1
	R1
	B1
	CH1
	B2
	B3
	JAIL
	C1
	U1
	C2
	C3
	R2
	D1
	CC2
	D2
	D3
	FP
	E1
	CH2
	E2
	E3
	R3
	F1
	F2
	U2
	F3
	G2J
	G1
	G2
	CC3
	G3
	R4
	CH3
	H1
	T2
	H2
	// number of squares
	NUMBER_OF_SQUARES
	// community chest, chance card
	NOP
	RAILWAY
	UTILITY
	BACK3
)

type pile struct {
	cards []int
	idx   int
}

func (p *pile) shuffle() {
	rand.Shuffle(len(p.cards), func(i, j int) {
		p.cards[i], p.cards[j] = p.cards[j], p.cards[i]
	})
	p.idx = 0
}

type communityChest struct {
	pile
}

func (c *communityChest) get(sq int) int {
	tmp := c.pile.cards[c.idx]
	c.pile.idx = (c.pile.idx + 1) % len(c.pile.cards)

	if tmp == NOP {
		return sq
	} else {
		return tmp
	}
}

type chanceCard struct {
	pile
}

func (c *chanceCard) get(sq int) int {
	nextR := func(sq int) int {
		switch sq {
		case CH1:
			return R2
		case CH2:
			return R3
		case CH3:
			return R1
		}

		panic("bad square")
	}

	nextU := func(sq int) int {
		switch sq {
		case CH1:
			return U1
		case CH2:
			return U2
		case CH3:
			return U1
		}

		panic("bad square")
	}

	tmp := c.pile.cards[c.pile.idx]
	c.pile.idx = (c.pile.idx + 1) % len(c.pile.cards)

	switch tmp {
	case NOP:
		return sq
	case GO, JAIL, C1, E3, H2, R1:
		return tmp
	case RAILWAY:
		return nextR(sq)
	case UTILITY:
		return nextU(sq)
	case BACK3:
		return (sq + 37) % NUMBER_OF_SQUARES
	}

	panic("invalid card")
}

type twoDice struct {
	side      int
	doubleCnt int
}

func (p *twoDice) roll(sq int) int {
	d1 := rand.IntN(p.side) + 1
	d2 := rand.IntN(p.side) + 1
	if d1 == d2 {
		if p.doubleCnt == 2 {
			p.doubleCnt = 0
			return JAIL
		}
		p.doubleCnt++
	} else {
		p.doubleCnt = 0
	}

	return (sq + d1 + d2) % NUMBER_OF_SQUARES
}

func compute(side, nAttempts int) string {
	// Monte Carlo method
	cChest := communityChest{pile{cards: []int{
		GO, JAIL, NOP, NOP, NOP, NOP, NOP, NOP,
		NOP, NOP, NOP, NOP, NOP, NOP, NOP, NOP,
	}}}
	cChest.shuffle()
	cCard := chanceCard{pile{cards: []int{
		GO, JAIL, C1, E3, H2, R1, RAILWAY, RAILWAY,
		UTILITY, BACK3, NOP, NOP, NOP, NOP, NOP, NOP,
	}}}
	cCard.shuffle()
	dice := twoDice{side: side}

	counterTbl := make([]int, NUMBER_OF_SQUARES)
	currentSQ := GO
	var nextSQ int
	for range nAttempts {
		nextSQ = dice.roll(currentSQ)

		switch nextSQ {
		case G2J:
			nextSQ = JAIL
		case CC1, CC2, CC3:
			nextSQ = cChest.get(nextSQ)
		case CH1, CH2, CH3:
			nextSQ = cCard.get(nextSQ)
		}

		counterTbl[nextSQ]++
		currentSQ = nextSQ
	}

	// sort the squares in descending order of number of visits
	type square struct {
		idx int
		cnt int
	}
	board := make([]square, NUMBER_OF_SQUARES)
	for idx, cnt := range counterTbl {
		board[idx] = square{idx: idx, cnt: cnt}
	}
	slices.SortFunc(board, func(x, y square) int {
		return cmp.Compare(y.cnt, x.cnt) // descending order
	})

	return fmt.Sprintf("%02d%02d%02d", board[0].idx, board[1].idx, board[2].idx)
}

func Solve() string {
	return compute(4, 1_000_000)
}
