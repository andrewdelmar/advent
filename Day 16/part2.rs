use std::{
    cmp::Reverse,
    collections::{BTreeMap, BTreeSet, BinaryHeap},
    fs,
};

mod board;
use board::{Board, Coord};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
struct PathNode {
    score: usize, // Order by length first.

    pos: Coord,
    dir: Coord,
    path: BTreeSet<Coord>,
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
    let mut path = BTreeSet::new();
    path.insert(start);
    q.push(Reverse(PathNode {
        score: 0,

        pos: start,
        dir: Coord::EAST,
        path,
    }));

    let mut v = BTreeMap::new();
    let mut end_score = None;
    let mut end_paths = BTreeSet::new();

    while let Some(Reverse(n)) = q.pop() {
        if let Some(end_score) = end_score {
            if n.score > end_score {
                break;
            }
        }

        if board[n.pos] == 'E' {
            end_score = Some(n.score);
            end_paths = end_paths.union(&n.path).cloned().collect();
        }

        let v_key = (n.pos, n.dir);
        if let Some(v_score) = v.get(&v_key) {
            if n.score > *v_score {
                continue;
            }
        }
        v.insert(v_key, n.score);

        let f = n.pos + n.dir;
        if board.contains(f) && board[f] != '#' {
            let mut f_path = n.path.clone();
            f_path.insert(f);

            q.push(Reverse(PathNode {
                score: n.score + 1,
                pos: f,
                dir: n.dir,
                path: f_path,
            }));
        }

        for t in turn(n.dir) {
            q.push(Reverse(PathNode {
                score: n.score + 1000,
                pos: n.pos,
                dir: t,
                path: n.path.clone(),
            }));
        }
    }

    end_paths.len()
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");

    let board = Board::load_str(&input);

    let start = start(&board);

    let score = min_score(start, &board);
    println!("Path Tiles: {}", score)
}
