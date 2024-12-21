package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

var afterCol = regexp.MustCompile(".*: (.*)$")

func parseRegister(line string) int64 {
	ns := afterCol.FindStringSubmatch(line)[1]
	n, err := strconv.ParseInt(ns, 10, 64)
	if err != nil {
		panic("Failed to parse register")
	}
	return n
}

type CPU struct {
	ip      int64
	a, b, c int64
	prog    []int64
}

func (c *CPU) literal() int64 {
	return c.prog[c.ip+1]
}

func (c *CPU) combo() (op int64) {
	switch c.prog[c.ip+1] {
	case 0, 1, 2, 3:
		op = c.prog[c.ip+1]
	case 4:
		op = c.a
	case 5:
		op = c.b
	case 6:
		op = c.c
	default:
		panic("invalid operand")
	}
	return
}

func (c *CPU) run() (out []int64) {
	for int(c.ip) < len(c.prog) {
		switch c.prog[c.ip] {
		case 0:
			c.a = c.a / (1 << c.combo())
		case 1:
			c.b = c.b ^ c.literal()
		case 2:
			c.b = c.combo() % 8
		case 3:
			if c.a != 0 {
				c.ip = c.literal()
				continue
			}
		case 4:
			c.b = c.b ^ c.c
		case 5:
			out = append(out, c.combo()%8)
		case 6:
			c.b = c.a / (1 << c.combo())
		case 7:
			c.c = c.a / (1 << c.combo())
		}
		c.ip += 2
	}
	return
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic("Failed to open input")
	}

	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	// Skip A.

	scanner.Scan()
	b := parseRegister(scanner.Text())

	scanner.Scan()
	c := parseRegister(scanner.Text())

	scanner.Scan()
	scanner.Scan()
	ps := afterCol.FindStringSubmatch(scanner.Text())[1]
	var prog []int64
	for _, s := range strings.Split(ps, ",") {
		n, err := strconv.ParseInt(s, 10, 64)
		if err != nil {
			panic("Failed to parse register")
		}
		prog = append(prog, n)
	}

	// By examining the program you can see it slowly shifts A to the right 3
	// bits at a time until it's zero.
	//
	// That means the last output only depends on the 3 most significant bits,
	// and the second last output then depends on the 6 MSB etc.
	//
	// So, we can cycle though guesses by changing 3 bits at a time from MSB
	// to LSB.
	var guessBits int64
	currInd := len(prog) - 1

	for currInd >= 0 {
		currShift := int64(currInd) * 3
		currMask := (int64(0b111) << currShift)
		currBits := (guessBits & currMask) >> currShift
		currBits++

		// If we are above 0b111 we have run out of bits to change and need to
		// retreat to a previous set of bits.
		if currBits > 0b111 {
			// Clear all the bits to the right of current shift.
			ds := currShift
			for ds >= 0 {
				dm := (int64(0b111) << ds)
				guessBits = guessBits & ^dm
				ds -= 3
			}

			currInd++
			continue
		}

		guessBits = (guessBits & ^currMask) | (currBits << currShift)

		cpu := CPU{
			ip:   0,
			a:    guessBits,
			b:    b,
			c:    c,
			prog: prog,
		}
		out := cpu.run()

		// Advance for every correct output.
		for currInd >= 0 && out[currInd] == prog[currInd] {
			currInd--
		}
	}

	fmt.Printf("Minimum A %d\n", guessBits)
}
