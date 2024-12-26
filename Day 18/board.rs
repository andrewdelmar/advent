use std::{
    cmp::Reverse,
    collections::BinaryHeap,
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
    pub const CARDINALS: [Self; 4] = [Self::NORTH, Self::EAST, Self::SOUTH, Self::WEST];
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

impl<T> Board<T>
where
    T: Clone,
{
    pub fn initialize(size: Coord, val: T) -> Self {
        Self {
            elems: vec![vec![val; size.x as usize]; size.y as usize],
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct ToVisitNode {
    pub score: usize,
    pub cost: usize,
    pub coord: Coord,
    pub prev: Option<Coord>,
}

#[derive(Clone, Copy)]
pub struct VisitedNode {
    pub cost: usize,
    pub prev: Option<Coord>,
}

impl<T> Board<T> {
    pub fn find_path<A>(&self, start: ToVisitNode, end: Coord, adjacent: A) -> Vec<Coord>
    where
        A: Fn(&Self, Coord, usize) -> Vec<ToVisitNode>,
    {
        let mut visited = Board::<Option<VisitedNode>>::initialize(self.size(), None);
        let mut to_visit = BinaryHeap::new();

        to_visit.push(Reverse(start));

        while let Some(Reverse(curr)) = to_visit.pop() {
            if let Some(other) = visited[curr.coord] {
                if other.cost <= curr.cost {
                    continue;
                }
            }

            visited[curr.coord] = Some(VisitedNode {
                cost: curr.cost,
                prev: curr.prev,
            });

            if curr.coord == end {
                break;
            }

            for a in adjacent(self, curr.coord, curr.cost) {
                to_visit.push(Reverse(a));
            }
        }

        let mut out = Vec::new();
        let mut path_coord = end;
        while let Some(path) = visited[path_coord] {
            out.push(path_coord);

            if let Some(prev) = path.prev {
                path_coord = prev;
            } else {
                break;
            }
        }

        out
    }
}
