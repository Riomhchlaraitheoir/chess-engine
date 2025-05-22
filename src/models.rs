#![allow(dead_code)]

pub use bitboard::*;
pub use game::*;
pub use position::*;
use std::{
    fmt::Display,
    ops::{Neg, Not},
};

mod bitboard;
mod game;
mod position;

#[derive(Debug, PartialEq, Eq)]
#[deprecated]
pub struct BoardState {
    pub pieces: Vec<Piece>,
    pub next_move: Colour,
    pub castling_rights: CastlingRights,
    pub en_passant: Option<Position>,
    pub half_moves: u32,
    pub full_moves: u32,
    pub status: Status,
    pub history: Vec<Move>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Piece {
    pub colour: Colour,
    pub piece_type: PieceType,
    pub position: Position,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Colour {
    White,
    Black,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum PieceType {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}

impl PieceType {
    pub const ALL: [Self; 6] = [
        Self::King,
        Self::Queen,
        Self::Rook,
        Self::Bishop,
        Self::Knight,
        Self::Pawn,
    ];
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CastlingRights {
    pub white_king: bool,
    pub white_queen: bool,
    pub black_king: bool,
    pub black_queen: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Move {
    pub from: Position,
    pub to: Position,
    pub move_type: MoveType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveType {
    Normal,
    DoublePawnPush,
    KingSideCastle,
    QueenSideCastle,
    Capture,
    EnPassant,
    KnightPromotion,
    BishopPromotion,
    RookPromotion,
    QueenPromotion,
    KnightPromotionCapture,
    BishopPromotionCapture,
    RookPromotionCapture,
    QueenPromotionCapture,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BoardUpdate {
    Remove(Position),
    Move {
        from: Position,
        to: Position,
        new_type: PieceType,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Direction {
    d_rank: i8,
    d_file: i8,
}

impl Not for Colour {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Self::White => Self::Black,
            Self::Black => Self::White,
        }
    }
}

impl From<usize> for PieceType {
    fn from(value: usize) -> Self {
        match value {
            0 => Self::King,
            1 => Self::Queen,
            2 => Self::Rook,
            3 => Self::Bishop,
            4 => Self::Knight,
            5 => Self::Pawn,
            _ => panic!("enum discriminant out of range"),
        }
    }
}

impl CastlingRights {
    pub fn queen_side(&self, colour: Colour) -> bool {
        match colour {
            Colour::White => self.white_queen,
            Colour::Black => self.black_queen,
        }
    }

    pub fn king_side(&self, colour: Colour) -> bool {
        match colour {
            Colour::White => self.white_king,
            Colour::Black => self.black_king,
        }
    }
}

impl Move {
    pub fn captured_position(&self) -> Option<Position> {
        if self.move_type.is_capture() {
            if self.move_type == MoveType::EnPassant {
                Some(Position {
                    rank: self.from.rank,
                    file: self.to.file,
                })
            } else {
                Some(self.to)
            }
        } else {
            None
        }
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.from.to_string())?;
        f.write_str(&self.to.to_string())?;
        if let Some(kind) = self.move_type.promote_to() {
            let char = match kind {
                PieceType::Queen => 'k',
                PieceType::Bishop => 'b',
                PieceType::Knight => 'n',
                PieceType::Rook => 'r',
                _ => return Ok(()),
            };
            f.write_str(&char.to_string())?;
        }
        Ok(())
    }
}

impl MoveType {
    pub fn promote_to(&self) -> Option<PieceType> {
        match self {
            Self::KnightPromotion | Self::KnightPromotionCapture => Some(PieceType::Knight),
            Self::BishopPromotion | Self::BishopPromotionCapture => Some(PieceType::Bishop),
            Self::RookPromotion | Self::RookPromotionCapture => Some(PieceType::Rook),
            Self::QueenPromotion | Self::QueenPromotionCapture => Some(PieceType::Queen),
            _ => None,
        }
    }

    pub fn is_capture(&self) -> bool {
        matches!(
            self,
            Self::Capture
                | Self::EnPassant
                | Self::KnightPromotionCapture
                | Self::BishopPromotionCapture
                | Self::RookPromotionCapture
                | Self::QueenPromotionCapture
        )
    }
}

impl Neg for Direction {
    type Output = Direction;

    fn neg(self) -> Self::Output {
        Direction {
            d_rank: -self.d_rank,
            d_file: -self.d_file,
        }
    }
}

impl Direction {
    pub fn between(from: &Position, to: &Position) -> Option<Direction> {
        if from == to {
            return None;
        }
        let d_rank = to.rank - from.rank;
        let d_file = to.file - from.file;
        let dir = if d_rank == 0 {
            if d_file < 0 { Self::LEFT } else { Self::RIGHT }
        } else if d_file == 0 {
            if d_rank < 0 { Self::DOWN } else { Self::UP }
        } else if from.rank + from.file == to.rank + to.file {
            if d_file < 0 {
                Self::LEFT_UP
            } else {
                Self::RIGHT_DOWN
            }
        } else if from.rank - from.file == to.rank - to.file {
            if d_file < 0 {
                Self::LEFT_DOWN
            } else {
                Self::RIGHT_UP
            }
        } else {
            return None;
        };
        Some(dir)
    }

    pub fn is_diagonal(&self) -> bool {
        self.d_rank != 0 && self.d_file != 0
    }

    pub fn is_between(&self, from: &Position, to: &Position) -> bool {
        if self.d_rank == 0 && from.rank != to.rank {
            return false;
        }
        if self.d_file == 0 && from.file != to.file {
            return false;
        }
        if self.d_rank == -1 && from.rank < to.rank {
            return false;
        }
        if self.d_rank == 1 && from.rank >= to.rank {
            return false;
        }
        if self.d_file == -1 && from.file < to.file {
            return false;
        }
        if self.d_file == 1 && from.file >= to.file {
            return false;
        }
        if self.d_file + self.d_rank == 0 && from.rank + from.file != to.rank + to.file {
            return false;
        }
        if self.d_file - self.d_rank == 0 && from.rank - from.file != to.rank - to.file {
            return false;
        }
        true
    }

    pub fn step(&self, from: &Position) -> Option<Position> {
        let mut copy = *from;
        copy.rank += self.d_rank;
        copy.file += self.d_file;
        if copy.out_of_bounds() {
            None
        } else {
            Some(copy)
        }
    }

    pub const LEFT: Direction = Direction {
        d_rank: 0,
        d_file: -1,
    };
    pub const RIGHT: Direction = Direction {
        d_rank: 0,
        d_file: 1,
    };
    pub const UP: Direction = Direction {
        d_rank: 1,
        d_file: 0,
    };
    pub const DOWN: Direction = Direction {
        d_rank: -1,
        d_file: 0,
    };
    pub const LEFT_UP: Direction = Direction {
        d_rank: 1,
        d_file: -1,
    };
    pub const LEFT_DOWN: Direction = Direction {
        d_rank: -1,
        d_file: -1,
    };
    pub const RIGHT_UP: Direction = Direction {
        d_rank: 1,
        d_file: 1,
    };
    pub const RIGHT_DOWN: Direction = Direction {
        d_rank: -1,
        d_file: 1,
    };
    pub const DIAGONALS: [Direction; 4] = [
        Self::LEFT_UP,
        Self::RIGHT_UP,
        Self::LEFT_DOWN,
        Self::RIGHT_DOWN,
    ];
    pub const NON_DIAGONAL: [Direction; 4] = [Self::LEFT, Self::RIGHT, Self::DOWN, Self::UP];
    pub const ALL: [Direction; 8] = [
        Self::UP,
        Self::DOWN,
        Self::LEFT,
        Self::RIGHT,
        Self::LEFT_UP,
        Self::LEFT_DOWN,
        Self::RIGHT_UP,
        Self::RIGHT_DOWN,
    ];
}
