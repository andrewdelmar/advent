use std::{fs, vec::Vec};

struct Seg {
    start: u32,
    size: u32,
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");

    let mut space = Vec::new();
    let mut data = Vec::new();

    let mut start = 0;
    let mut is_data = true;
    for size in input.chars().flat_map(|c| c.to_digit(10)) {
        if is_data {
            data.push(Seg { start, size });
        } else {
            space.push(Seg { start, size });
        }

        start += size;
        is_data = !is_data;
    }

    let mut total: u64 = 0;
    for (id, mut d) in data.into_iter().enumerate().rev() {
        for s in space.iter_mut() {
            if s.start < d.start && s.size >= d.size {
                d.start = s.start;
                s.size -= d.size;
                s.start += d.size;
            }
        }

        for addr in d.start..d.start + d.size {
            total += id as u64 * addr as u64;
        }
    }

    println!("Total: {}", total);
}
