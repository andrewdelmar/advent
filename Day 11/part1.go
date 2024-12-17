package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	bytes, err := os.ReadFile("input.txt")
	if err != nil {
		panic("Failed to open input")
	}

	var total int64
	parts := strings.Split(string(bytes), " ")
	for _, s := range parts {
		rocks := make(map[int64]int64)
		start, err := strconv.ParseInt(strings.Trim(s, " \n"), 10, 64)
		if err != nil {
			panic("Failed to parse rock")
		}
		rocks[start] += 1

		for i := 0; i < 25; i++ {
			nr := make(map[int64]int64)
			for num, count := range rocks {
				nums := strconv.FormatInt(num, 10)
				switch {
				case num == 0:
					nr[1] += count
				case len(nums)%2 == 0:
					a, _ := strconv.ParseInt(nums[:len(nums)/2], 10, 64)
					nr[a] += count
					b, _ := strconv.ParseInt(nums[len(nums)/2:], 10, 64)
					nr[b] += count
				default:
					nr[num*2024] += count
				}
			}

			rocks = nr
		}

		for _, count := range rocks {
			total += count
		}
	}

	fmt.Printf("Num Rocks: %d", total)
}
