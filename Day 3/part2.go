package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
)

var mullPat = regexp.MustCompile("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don't\\(\\)")

func main() {
	bytes, err := os.ReadFile("input.txt")
	if err != nil {
		panic("Failed to open input")
	}

	matches := mullPat.FindAllStringSubmatch(string(bytes), -1)

	total := 0
	do := true
	for _, match := range matches {
		switch match[0][0:3] {
		case "mul":
			if do {
				a, err := strconv.Atoi(match[1])
				if err != nil {
					panic("Failed to parse a")
				}
				b, err := strconv.Atoi(match[2])
				if err != nil {
					panic("Failed to parse b")
				}

				total += a * b
			}
		case "do(":
			do = true
		case "don":
			do = false
		}

	}
	fmt.Printf("total: %d\n", total)
}
