use std::{
    ops,
    ops::{Index, IndexMut},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub struct Coord {
    pub x: i32,
    pub y: i32,
}

impl Coord {
    pub const NORTH: Self = Self { x: 0, y: -1 };
    pub const EAST: Self = Self { x: 1, y: 0 };
    pub const SOUTH: Self = Self { x: 0, y: 1 };
    pub const WEST: Self = Self { x: -1, y: 0 };
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

#[derive(Clone, Default)]
pub struct Board<T> {
    elems: Vec<Vec<T>>,
}

impl<T> Index<Coord> for Board<T> {
    type Output = T;

    fn index(&self, index: Coord) -> &Self::Output {
        &self.elems[index.y as usize][index.x as usize]
    }
}

impl<T> IndexMut<Coord> for Board<T> {
    fn index_mut(&mut self, index: Coord) -> &mut Self::Output {
        &mut self.elems[index.y as usize][index.x as usize]
    }
}

impl Board<char> {
    pub fn load_str(lines: &str) -> Self {
        let mut out = Self::default();
        for line in lines.lines() {
            out.elems.push(line.chars().collect());
        }
        out
    }
}

impl<T> Board<T> {
    pub fn contains(&self, c: Coord) -> bool {
        let s = self.size();
        c.x >= 0 && c.x < s.x && c.y >= 0 && c.y < s.y
    }

    pub fn size(&self) -> Coord {
        Coord {
            x: self.elems[0].len() as i32,
            y: self.elems.len() as i32,
        }
    }

    pub fn iter_enum(&self) -> impl Iterator<Item = (Coord, &T)> {
        self.elems.iter().enumerate().flat_map(move |(y, r)| {
            r.iter().enumerate().map(move |(x, e)| {
                (
                    Coord {
                        x: x as i32,
                        y: y as i32,
                    },
                    e,
                )
            })
        })
    }
}
