package main

import (
	"bufio"
	"fmt"
	"os"
)

type coord [2]int

func findPeaks(board [][]int, height int, c coord, paths *int) {
	x := c[0]
	y := c[1]

	if x < 0 || x >= len(board[0]) || y < 0 || y >= len(board) {
		return
	}

	if board[y][x] != height {
		return
	}

	if height == 9 {
		*paths += 1
		return
	}

	findPeaks(board, height+1, coord{x + 1, y}, paths)
	findPeaks(board, height+1, coord{x - 1, y}, paths)
	findPeaks(board, height+1, coord{x, y + 1}, paths)
	findPeaks(board, height+1, coord{x, y - 1}, paths)
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		panic("Failed to open input")
	}

	defer file.Close()

	var board [][]int

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		var line []int

		for _, r := range []rune(scanner.Text()) {
			line = append(line, int(r)-int('0'))
		}

		board = append(board, line)
	}

	var total int
	for y, line := range board {
		for x, height := range line {
			if height == 0 {
				var paths int
				findPeaks(board, 0, coord{x, y}, &paths)
				total += paths
			}
		}
	}

	fmt.Printf("Total: %d\n", total)
}
