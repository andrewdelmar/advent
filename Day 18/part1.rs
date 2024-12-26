mod board;
use board::{Board, Coord, ToVisitNode};

use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() {
    let file = File::open("input.txt").expect("Failed to open input.txt");
    let reader = BufReader::new(file);

    let mut coords = Vec::new();
    for (i, line) in reader.lines().enumerate() {
        if i >= 1024 {
            break;
        }
        let line = line.expect("Failed to read line");
        let mut parts = line.split(",");
        let x: i32 = parts
            .next()
            .expect("Missing coord")
            .parse()
            .expect("Failed to parse coord");
        let y: i32 = parts
            .next()
            .expect("Missing coord")
            .parse()
            .expect("Failed to parse coord");

        coords.push(Coord { x, y });
    }

    let mut board = Board::initialize(Coord { x: 71, y: 71 }, false);
    for coord in coords {
        board[coord] = true;
    }

    fn adj(board: &Board<bool>, coord: Coord, cost: usize) -> Vec<ToVisitNode> {
        let mut out = Vec::new();
        for dir in Coord::CARDINALS {
            let n_coord = coord + dir;

            let n_cost = cost + 1;
            let h = (70 - n_coord.x).abs() + (70 - n_coord.y).abs();

            if board.contains(n_coord) && !board[n_coord] {
                out.push(ToVisitNode {
                    score: n_cost + h as usize,
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
        coord: Coord { x: 0, y: 0 },
        prev: None,
    };

    let path = board.find_path(start, Coord { x: 70, y: 70 }, adj);
    println!("Steps to End: {}", path.len() - 1);
}
