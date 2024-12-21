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

func correctVals(prog, out []int64) int {
	for i := range out {
		if prog[i] != out[i] {
			return i
		}
	}

	return len(out)
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic("Failed to open input")
	}

	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	var a int64

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

	// By examining the program you can see it's output depends on processing A
	// 3 bits at a from time LSB to MSB.
	//
	// So, we can progressively lock in the bits of A as we correctly guess the
	// output. The output depends on the lower 3-11 bits of A so we can only
	// lock in bits when we have 4 correct values (12 bits of data).
	var guessVal, correctBits, correctVal int64
	var bestGuess int

	for {
		a = (guessVal << correctBits) | correctVal

		cpu := CPU{
			ip:   0,
			a:    a,
			b:    b,
			c:    c,
			prog: prog,
		}

		out := cpu.run()

		guess := correctVals(prog, out)
		if guess == len(prog) {
			break
		}

		if guess > bestGuess+4 {

			bestGuess++

			correctBits += 3
			badVals := ((^int64(0) >> correctBits) << correctBits)
			correctVal = a & ^badVals

			fmt.Printf("New best guess, A: %d, out: %v, correct bits: %d\n", a, out, correctBits)

			guessVal = 0
		}

		guessVal++
	}

	fmt.Printf("Minimum A %d\n", a)
}
