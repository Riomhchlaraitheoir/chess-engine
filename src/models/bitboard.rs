use crate::models::Position;
use core::fmt::Debug;
use derive_more::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};
use std::fmt::Display;
use std::ops::{Add, AddAssign, BitOr};

#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    Default,
    BitOr,
    BitAnd,
    BitXor,
    BitOrAssign,
    BitAndAssign,
    BitXorAssign,
    Not,
)]
pub struct BitBoard(u64);

impl BitBoard {
    pub const fn const_default() -> Self {
        Self(0)
    }

    pub const fn const_from(value: u64) -> Self {
        Self(value)
    }

    pub fn new(positions: impl IntoIterator<Item = Position>) -> Self {
        let mut board = Self::default();
        for position in positions {
            board |= position.into()
        }
        board
    }

    pub fn iter(&self) -> impl Iterator<Item = Position> + use<> {
        BitBoardIterator(self.0)
    }

    pub fn toggle(&mut self, position: Position) {
        *self ^= position.into()
    }

    pub fn contains(self, pos: Position) -> bool {
        !(self & Self::from(pos)).is_empty()
    }

    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub const fn from_positions<const N: usize>(positions: [Position; N]) -> Self {
        let mut board = 0;
        let mut i = 0;
        while i < N {
            board |= 1 << positions[i].index();
            i += 1;
        }
        BitBoard(board)
    }
}

impl From<Position> for BitBoard {
    fn from(value: Position) -> Self {
        Self(1u64 << value.index())
    }
}

impl Debug for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("BitBoard({:064b})", self.0))
    }
}

pub struct BitBoardIterator(u64);

impl Iterator for BitBoardIterator {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0 {
            return None;
        }
        let index = self.0.trailing_zeros() as usize;
        let position = Position::from_index(index);
        self.0 ^= BitBoard::from(position).0;
        Some(position)
    }
}

impl FromIterator<(i8, i8)> for BitBoard {
    fn from_iter<T: IntoIterator<Item = (i8, i8)>>(iter: T) -> Self {
        iter.into_iter().map(Position::from).collect()
    }
}

impl FromIterator<Position> for BitBoard {
    fn from_iter<T: IntoIterator<Item = Position>>(positions: T) -> Self {
        let mut board = Self::default();
        for position in positions {
            board |= position.into()
        }
        board
    }
}

impl FromIterator<BitBoard> for BitBoard {
    fn from_iter<T: IntoIterator<Item = BitBoard>>(positions: T) -> Self {
        positions
            .into_iter()
            .fold(Self::default(), <Self as BitOr>::bitor)
    }
}

impl IntoIterator for BitBoard {
    type Item = Position;
    type IntoIter = BitBoardIterator;

    fn into_iter(self) -> Self::IntoIter {
        BitBoardIterator(self.0)
    }
}

impl Display for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ranks: [u8; 8] = self.0.to_be_bytes();
        for byte in ranks {
            let line: String = (0..8)
                .map(|i| if byte >> i & 1 > 0 { 'X' } else { '.' })
                .collect();
            f.write_str(&line)?;
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl Add<Position> for BitBoard {
    type Output = Self;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn add(self, rhs: Position) -> Self::Output {
        self | Self::from(rhs)
    }
}

impl AddAssign<Position> for BitBoard {
    #[allow(clippy::suspicious_op_assign_impl)]
    fn add_assign(&mut self, rhs: Position) {
        *self |= Self::from(rhs)
    }
}

#[cfg(test)]
mod test {
    use super::{BitBoard, Position};

    #[test]
    fn bitor_assign() {
        let mut board = BitBoard::default();
        board |= BitBoard(0xFF00FF00FF00FF00);
        assert_eq!(board.0, 0xFF00FF00FF00FF00);
        board |= BitBoard(0x00FF00FF00FF00FF);
        assert_eq!(board.0, u64::MAX);
    }

    #[test]
    fn iter() {
        {
            let board = BitBoard(0);
            let positions: Vec<_> = board.iter().collect();
            assert_eq!(positions, vec![]);
        }
        {
            let board = BitBoard(u64::MAX);
            let positions: Vec<_> = board.iter().collect();
            let expected: Vec<_> = (0..64).map(Position::from_index).collect();
            assert_eq!(positions, expected)
        }
        {
            let board = BitBoard(0xAAAAAAAAAAAAAAAA);
            let positions: Vec<_> = board.iter().collect();
            let expected: Vec<_> = (0..64)
                .filter(|i| i % 2 == 1)
                .map(Position::from_index)
                .collect();
            assert_eq!(positions, expected)
        }
    }

    #[test]
    fn from_iter() {
        let board: BitBoard = (0..8)
            .flat_map(move |rank| (0..8).map(move |file| Position { rank, file }))
            .collect();
        assert_eq!(board.0, u64::MAX)
    }
}
