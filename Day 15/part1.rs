use std::{collections::VecDeque, fs};

fn move_dude((rx, ry): (i32, i32), (dx, dy): (i32, i32), board: &mut Vec<Vec<char>>) -> (i32, i32) {
    let mut px = rx + dx;
    let mut py = ry + dy;
    while board[py as usize][px as usize] == 'O' {
        px += dx;
        py += dy;
    }

    if board[py as usize][px as usize] == '#' {
        return (rx, ry);
    }

    board[py as usize][px as usize] = 'O';
    let nx = rx + dx;
    let ny = ry + dy;
    board[ny as usize][nx as usize] = '.';
    (nx, ny)
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");

    let mut parts = input.split("\n\n");
    let (Some(board_in), Some(instr_in)) = (parts.next(), parts.next()) else {
        panic!("No empty line!")
    };

    let mut board = Vec::new();
    let mut robot_pos = (0, 0);
    for (y, l) in board_in.lines().enumerate() {
        let mut row = Vec::new();
        for (x, c) in l.chars().enumerate() {
            let t = if c == '@' {
                robot_pos = (x as i32, y as i32);
                '.'
            } else {
                c
            };

            row.push(t);
        }
        board.push(row);
    }

    let mut instructions = VecDeque::new();
    for c in instr_in.chars() {
        if c != '\n' {
            instructions.push_back(c);
        }
    }

    while let Some(instr) = instructions.pop_front() {
        let dir = match instr {
            '^' => (0, -1),
            '>' => (1, 0),
            'v' => (0, 1),
            '<' => (-1, 0),
            _ => unreachable!(),
        };
        robot_pos = move_dude(robot_pos, dir, &mut board);
    }

    let mut total = 0;
    for (y, row) in board.iter().enumerate() {
        for (x, c) in row.iter().enumerate() {
            if *c == 'O' {
                total += x + y * 100;
            }
        }
    }

    println!("Total GPS Coords: {}", total);
}
