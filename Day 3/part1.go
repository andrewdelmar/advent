package main

import (
	"fmt"
	"os"
	"regexp"
	"strconv"
)

var mullPat = regexp.MustCompile("mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")

func main() {
	bytes, err := os.ReadFile("input.txt")
	if err != nil {
		panic("Failed to open input")
	}

	matches := mullPat.FindAllStringSubmatch(string(bytes), -1)

	total := 0
	for _, match := range matches {
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
	fmt.Printf("total: %d\n", total)
}
