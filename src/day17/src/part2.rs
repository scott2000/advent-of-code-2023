use pathfinding::directed::astar::astar;

const INPUT: &str = include_str!("../../../inputs/day17.txt");
const MIN_STRAIGHT: u8 = 4;
const MAX_STRAIGHT: u8 = 10;

type Grid = Vec<Vec<u32>>;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    fn possible_next(self) -> [Self; 3] {
        match self {
            Direction::North => [Direction::West, Direction::North, Direction::East],
            Direction::East => [Direction::North, Direction::East, Direction::South],
            Direction::South => [Direction::East, Direction::South, Direction::West],
            Direction::West => [Direction::South, Direction::West, Direction::North],
        }
    }

    fn offset(self) -> (isize, isize) {
        match self {
            Direction::North => (-1, 0),
            Direction::East => (0, 1),
            Direction::South => (1, 0),
            Direction::West => (0, -1),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
struct State {
    row: usize,
    col: usize,
    dir: Direction,
    straight_count: u8,
}

impl State {
    fn step(mut self, grid: &Grid, dir: Direction) -> Option<Self> {
        let (row_offset, col_offset) = dir.offset();

        let row = self.row as isize + row_offset;
        let col = self.col as isize + col_offset;

        if row < 0 || col < 0 || row >= (grid.len() as isize) || col >= (grid[0].len() as isize) {
            return None;
        }

        self.row = row as usize;
        self.col = col as usize;

        if self.straight_count == 0 {
            // Starting tile
            self.straight_count = 1;
        } else if self.dir == dir {
            if self.straight_count >= MAX_STRAIGHT {
                return None;
            }
            self.straight_count += 1;
        } else {
            if self.straight_count < MIN_STRAIGHT {
                return None;
            }
            self.straight_count = 1;
        }

        self.dir = dir;

        Some(self)
    }

    fn next(&self, grid: &Grid) -> Vec<(Self, u32)> {
        self.dir
            .possible_next()
            .into_iter()
            .filter_map(|dir| self.step(grid, dir))
            .map(|state| (state, grid[state.row][state.col]))
            .collect()
    }

    fn heuristic(&self, grid: &Grid) -> u32 {
        (self.row.abs_diff(grid.len()) + self.col.abs_diff(grid[0].len())) as u32
    }

    fn is_end(&self, grid: &Grid) -> bool {
        self.straight_count >= MIN_STRAIGHT
            && self.row == (grid.len() - 1)
            && self.col == (grid[0].len() - 1)
    }
}

impl Default for State {
    fn default() -> Self {
        Self {
            row: 0,
            col: 0,
            dir: Direction::East,
            straight_count: 0,
        }
    }
}

pub fn main() {
    let grid: Grid = INPUT
        .split('\n')
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(|line| line.chars().filter_map(|ch| ch.to_digit(10)).collect())
        .collect();

    let result = astar(
        &State::default(),
        |state| state.next(&grid),
        |state| state.heuristic(&grid),
        |state| state.is_end(&grid),
    );

    match result {
        None => println!("No solution!"),
        Some((_, cost)) => println!("{cost}"),
    }
}
