use std::{fs, vec::Vec};

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");

    let mut a = Vec::<i32>::new();
    let mut b = Vec::<i32>::new();
    for line in input.lines() {
        let mut parts = line.split_whitespace();
        let (Some(ap), Some(bp)) = (parts.next(), parts.next()) else {
            panic!("Malformed line")
        };

        a.push(ap.parse().expect("Failed to parse a"));
        b.push(bp.parse().expect("Failed to parse b"));
    }

    let mut score = 0;
    for av in a {
        let b_count = b.iter().filter(|bv| **bv == av).count();
        score += av * b_count as i32;
    }

    print!("Score: {}", score)
}
