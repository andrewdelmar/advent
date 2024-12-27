mod board;
use board::{Board, Coord, ToVisitNode};

use std::fs;

fn main() {
    let file = fs::read_to_string("input.txt").expect("Failed to open input.txt");
    let board = Board::load_str(&file);

    let mut s = Coord::default();
    let mut e = Coord::default();
    for (coord, cell) in board.iter_enum() {
        match *cell {
            'S' => s = coord,
            'E' => e = coord,
            _ => {}
        }
    }

    // There is only a single path so this is overkill.
    fn adj(board: &Board<char>, coord: Coord, cost: usize) -> Vec<ToVisitNode> {
        let mut out = Vec::new();
        for dir in Coord::CARDINALS {
            let n_coord = coord + dir;
            let n_cost = cost + 1;
            if board.contains(n_coord) && board[n_coord] != '#' {
                out.push(ToVisitNode {
                    score: n_cost,
                    cost: n_cost,
                    coord: n_coord,
                    prev: Some(coord),
                });
            }
        }
        out
    }

    let start = ToVisitNode {
        score: 0,
        cost: 0,
        coord: s,
        prev: None,
    };
    let path = board.find_path(start, e, adj);

    let mut times = Board::initialize(board.size(), 0);
    for (i, p) in path.iter().enumerate() {
        times[*p] = i;
    }

    let mut num_cheats = 0;
    const MAX_CHEAT: i32 = 20;
    for (i, p) in path.iter().enumerate() {
        for dy in -MAX_CHEAT..MAX_CHEAT + 1 {
            for dx in -MAX_CHEAT..MAX_CHEAT + 1 {
                let coord = *p + Coord { x: dx, y: dy };
                let cheat_dist = dx.abs() + dy.abs();
                if cheat_dist <= MAX_CHEAT
                    && times.contains(coord)
                    && times[coord] >= i + 100 + cheat_dist as usize
                {
                    num_cheats += 1;
                }
            }
        }
    }

    println!("Num Cheats: {}", num_cheats);
}
