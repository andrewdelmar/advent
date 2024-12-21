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
	a := parseRegister(scanner.Text())

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

	cpu := CPU{
		ip:   0,
		a:    a,
		b:    b,
		c:    c,
		prog: prog,
	}

	out := cpu.run()

	var os []string
	for _, n := range out {
		os = append(os, strconv.FormatInt(n, 10))
	}
	fmt.Printf("Output: %q\n", strings.Join(os, ","))
}
