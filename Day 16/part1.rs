use std::{
    cmp::Reverse,
    collections::{BTreeMap, BinaryHeap},
    fs,
};

mod board;
use board::{Board, Coord};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct PathNode {
    score: usize, // Order by length first.

    pos: Coord,
    dir: Coord,
}

fn turn(dir: Coord) -> [Coord; 2] {
    match dir {
        Coord::NORTH | Coord::SOUTH => [Coord::EAST, Coord::WEST],
        Coord::EAST | Coord::WEST => [Coord::NORTH, Coord::SOUTH],
        _ => unreachable!(),
    }
}

fn start(board: &Board<char>) -> Coord {
    let mut start = Coord::default();
    for (c, e) in board.iter_enum() {
        if *e == 'S' {
            start = c;
        }
    }

    start
}

fn min_score(start: Coord, board: &Board<char>) -> usize {
    let mut q = BinaryHeap::default();
    q.push(Reverse(PathNode {
        score: 0,

        pos: start,
        dir: Coord::EAST,
    }));

    let mut v = BTreeMap::<(Coord, Coord), usize>::new();

    while let Some(Reverse(n)) = q.pop() {
        let v_key = (n.pos, n.dir);
        if let Some(v_score) = v.get(&v_key) {
            if *v_score <= n.score {
                continue;
            }
        }
        v.insert(v_key, n.score);

        if board[n.pos] == 'E' {
            return n.score;
        }

        let f = n.pos + n.dir;
        if board.contains(f) && board[f] != '#' {
            q.push(Reverse(PathNode {
                score: n.score + 1,
                pos: f,
                dir: n.dir,
            }));
        }

        for t in turn(n.dir) {
            q.push(Reverse(PathNode {
                score: n.score + 1000,
                pos: n.pos,
                dir: t,
            }));
        }
    }

    0
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");

    let board = Board::load_str(&input);

    let start = start(&board);

    let score = min_score(start, &board);
    println!("Min Score: {}", score)
}
