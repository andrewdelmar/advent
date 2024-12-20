use std::{collections::VecDeque, fs, ops};

#[derive(Clone, Copy, PartialEq, Eq, Default)]
struct Coord {
    x: i32,
    y: i32,
}

impl Coord {
    const NORTH: Self = Self { x: 0, y: -1 };
    const EAST: Self = Self { x: 1, y: 0 };
    const SOUTH: Self = Self { x: 0, y: 1 };
    const WEST: Self = Self { x: -1, y: 0 };
}

impl ops::Add<Coord> for Coord {
    type Output = Coord;

    fn add(self, rhs: Coord) -> Coord {
        Coord {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

fn box_coords(c: char, pos: Coord) -> [Coord; 2] {
    let l = if c == '[' { pos } else { pos + Coord::WEST };

    [l, l + Coord::EAST]
}

fn push_coords(c: char, pos: Coord, dir: Coord) -> Vec<Coord> {
    if c != '[' && c != ']' {
        Vec::new()
    } else {
        let [l, r] = box_coords(c, pos);

        match dir {
            Coord::NORTH | Coord::SOUTH => vec![l + dir, r + dir],
            Coord::EAST => vec![r + Coord::EAST],
            Coord::WEST => vec![l + Coord::WEST],
            _ => unreachable!(),
        }
    }
}

fn can_push(pos: Coord, dir: Coord, board: &Vec<Vec<char>>) -> bool {
    match board[pos.y as usize][pos.x as usize] {
        '.' => true,
        '#' => false,
        c => push_coords(c, pos, dir)
            .into_iter()
            .all(|pp| can_push(pp, dir, board)),
    }
}

fn push(pos: Coord, dir: Coord, board: &mut Vec<Vec<char>>) {
    let c = board[pos.y as usize][pos.x as usize];

    if c == '.' || c == '#' {
        return;
    }

    let [l, r] = box_coords(c, pos);

    for pc in push_coords(c, pos, dir) {
        push(pc, dir, board);
    }

    let (nl, nr) = (l + dir, r + dir);
    board[l.y as usize][l.x as usize] = '.';
    board[r.y as usize][r.x as usize] = '.';
    board[nl.y as usize][nl.x as usize] = '[';
    board[nr.y as usize][nr.x as usize] = ']';
}

fn move_dude(pos: &mut Coord, dir: Coord, board: &mut Vec<Vec<char>>) {
    if can_push(*pos + dir, dir, &board) {
        push(*pos + dir, dir, board);
        *pos = *pos + dir;
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");

    let mut parts = input.split("\n\n");
    let (Some(board_in), Some(instr_in)) = (parts.next(), parts.next()) else {
        panic!("No empty line!")
    };

    let mut board = Vec::new();
    let mut robot_pos = Coord::default();
    for (y, line) in board_in.lines().enumerate() {
        let mut row = Vec::new();
        for (x, c) in line.chars().enumerate() {
            let (l, r) = match c {
                'O' => ('[', ']'),
                '@' => {
                    robot_pos = Coord {
                        x: x as i32 * 2,
                        y: y as i32,
                    };
                    ('.', '.')
                }
                c => (c, c),
            };

            row.push(l);
            row.push(r);
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
            '^' => Coord::NORTH,
            '>' => Coord::EAST,
            'v' => Coord::SOUTH,
            '<' => Coord::WEST,
            _ => unreachable!(),
        };

        move_dude(&mut robot_pos, dir, &mut board);
    }

    let mut total = 0;
    for (y, row) in board.iter().enumerate() {
        for (x, c) in row.iter().enumerate() {
            if *c == '[' {
                total += x + y * 100;
            }
        }
    }

    println!("Total GPS Coords: {}", total);
}
