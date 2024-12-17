use std::{fs, vec::Vec};

fn main() {
    let input = fs::read_to_string("input.txt").expect("Failed to read input.txt");

    let values = input.chars().flat_map(|c| c.to_digit(10));
    let data: Vec<u32> = values.clone().step_by(2).collect();
    let space: Vec<u32> = values.skip(1).step_by(2).collect();

    let mut space_i = 0;
    let mut space_addr = 0;
    let mut space_left = space[space_i];
    let mut skip_left = data[space_i];

    let mut data_i = data.len() - 1;
    let mut data_addr = data.iter().sum::<u32>() + space.iter().sum::<u32>() - 1;
    let mut data_left = data[data_i];

    let mut total: u64 = 0;

    while space_addr <= data_addr {
        if skip_left > 0 {
            total += space_i as u64 * space_addr as u64;
            skip_left -= 1;
            space_addr += 1;
        } else if data_left == 0 {
            data_i -= 1;
            data_addr -= space[data_i];
            data_left = data[data_i];
        } else if space_left == 0 {
            space_i += 1;
            skip_left = data[space_i];
            space_left += space[space_i];
        } else {
            total += data_i as u64 * space_addr as u64;
            data_left -= 1;
            data_addr -= 1;
            space_left -= 1;
            space_addr += 1;
        }
    }

    println!("Total: {}", total);
}
