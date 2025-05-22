use std::char::TryFromCharError;
use std::fmt::Display;
use std::str::FromStr;

use crate::Colour;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Position {
    // TODO change to u8
    pub rank: i8,
    pub file: i8,
}

impl Position {
    pub const fn index(&self) -> usize {
        ((self.rank * 8) + self.file) as usize
    }
    pub const fn from_index(value: usize) -> Self {
        Position {
            rank: (value / 8) as i8,
            file: (value % 8) as i8,
        }
    }

    pub fn tile_colour(&self) -> Colour {
        if (self.rank + self.file) % 2 != 0 {
            Colour::White
        } else {
            Colour::Black
        }
    }

    pub fn out_of_bounds(&self) -> bool {
        !self.in_bounds()
    }

    pub fn in_bounds(&self) -> bool {
        0 <= self.rank && self.rank < 8 && 0 <= self.file && self.file < 8
    }

    pub fn all() -> impl Iterator<Item = Self> {
        (0i8..8).flat_map(|rank| (0i8..8).map(move |file| Self { rank, file }))
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let file = ((('a' as i8) + self.file) as u8) as char;
        f.write_str(&file.to_string())?;
        f.write_str(&(self.rank + 1).to_string())?;
        Ok(())
    }
}

impl From<usize> for Position {
    fn from(value: usize) -> Self {
        Self::from_index(value)
    }
}

impl From<Position> for usize {
    fn from(value: Position) -> Self {
        value.index()
    }
}

impl From<(i8, i8)> for Position {
    fn from((rank, file): (i8, i8)) -> Self {
        Self { rank, file }
    }
}

#[derive(Debug, Error)]
pub enum PositionParseError {
    #[error("Incorrect string length: {0}, expected: 2")]
    BadLen(usize),
    #[error("Invalid position")]
    OutOfRange,
    #[error(transparent)]
    ExpectedDigit(#[from] TryFromCharError),
}

impl FromStr for Position {
    type Err = PositionParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 2 {
            return Err(PositionParseError::BadLen(s.len()));
        }
        let mut chars = s.chars();
        let file_char: u8 = chars
            .next()
            .ok_or(PositionParseError::BadLen(0))?
            .try_into()?;
        let rank_char: u8 = chars
            .next()
            .ok_or(PositionParseError::BadLen(1))?
            .try_into()?;
        let file = file_char - u8::try_from('a')?;
        let rank = rank_char - u8::try_from('1')?;
        let pos = Position {
            rank: rank as i8,
            file: file as i8,
        };
        if pos.out_of_bounds() {
            return Err(PositionParseError::OutOfRange);
        }
        Ok(pos)
    }
}

#[cfg(test)]
mod test {
    use crate::{Colour, Position};
    use itertools::Itertools;

    #[test]
    fn index() {
        let indices: Vec<_> = (0..8)
            .flat_map(move |rank| (0..8).map(move |file| Position { rank, file }.index()))
            .collect();
        assert_eq!(indices, (0..64).collect_vec());
    }

    #[test]
    fn tile_colour() {
        // a1 should be black
        let colour = Position { rank: 0, file: 0 }.tile_colour();
        assert_eq!(colour, Colour::Black)
    }
}
