use std::fmt::Display;
use std::num::ParseIntError;
use std::ops::{Index, IndexMut};
use std::str::FromStr;

use thiserror::Error;

use crate::PiecesParseError;
use crate::{BitBoard, CastlingRights, Colour, Move, PieceType, Position, PositionParseError};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Game {
    pub white_pieces: PieceSet,
    pub black_pieces: PieceSet,
    pub current_player: Colour,
    pub castling_rights: CastlingRights,
    pub en_passant: Option<Position>,
    pub half_moves: u32,
    pub full_moves: u32,
    // history is a list of previous moves, if castling rights are changed by a move,
    // the old rights will be the second element of the tuple
    pub history: Vec<(Move, Option<CastlingRights>)>,
    pub captured_pieces: Vec<PieceType>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct PieceSet([BitBoard; 6]);

impl Index<PieceType> for PieceSet {
    type Output = BitBoard;

    fn index(&self, index: PieceType) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl IndexMut<PieceType> for PieceSet {
    fn index_mut(&mut self, index: PieceType) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

impl TryFrom<[BitBoard; 6]> for PieceSet {
    type Error = ();

    fn try_from(value: [BitBoard; 6]) -> Result<Self, Self::Error> {
        if value[0].into_iter().count() != 1 {
            Err(())
        } else {
            Ok(Self(value))
        }
    }
}

impl PieceSet {
    pub fn king(&self) -> Position {
        #[allow(clippy::expect_used)]
        self.0[0].into_iter().next().expect("King not found")
    }

    pub fn remove_from(&mut self, position: Position) {
        let mask: BitBoard = position.into();
        let mask = !mask;
        for board in self.0.iter_mut() {
            *board &= mask;
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (PieceType, BitBoard)> + '_ {
        (0..6).map(|i| (PieceType::from(i), self.0[i]))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (PieceType, &'_ mut BitBoard)> + '_ {
        self.0
            .iter_mut()
            .enumerate()
            .map(|(i, b)| (PieceType::from(i), b))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Status {
    Normal,
    Check { colour: Colour, attacker: Position },
    Checkmate { colour: Colour, attacker: Position },
    Stalemate,
}

impl Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&"_".repeat(17))?;
        f.write_str("\n")?;
        let board = self.build_array();
        for rank in (0..8).rev() {
            for file in 0..8 {
                let position = Position { rank, file };
                let piece = board[position.index()];
                f.write_str("|")?;
                match piece {
                    Some((colour, kind)) => {
                        let piece_char: char = match (colour, kind) {
                            (Colour::Black, PieceType::King) => '\u{2654}',
                            (Colour::Black, PieceType::Queen) => '\u{2655}',
                            (Colour::Black, PieceType::Rook) => '\u{2656}',
                            (Colour::Black, PieceType::Bishop) => '\u{2657}',
                            (Colour::Black, PieceType::Knight) => '\u{2658}',
                            (Colour::Black, PieceType::Pawn) => '\u{2659}',
                            (Colour::White, PieceType::King) => '\u{265A}',
                            (Colour::White, PieceType::Queen) => '\u{265B}',
                            (Colour::White, PieceType::Rook) => '\u{265C}',
                            (Colour::White, PieceType::Bishop) => '\u{265D}',
                            (Colour::White, PieceType::Knight) => '\u{265E}',
                            (Colour::White, PieceType::Pawn) => '\u{265F}',
                        };
                        f.write_str(&format!("{piece_char}"))?
                    }
                    None => f.write_str(" ")?,
                }
            }
            f.write_str("|\n")?;
            f.write_str(&"_".repeat(17))?;
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl Default for Game {
    fn default() -> Self {
        DEFAULT.clone()
    }
}

impl Game {
    pub fn new() -> Self {
        Self::default()
    }
}

#[macro_export]
macro_rules! position {
    (a1) => {
        $crate::Position { rank: 0, file: 0 }
    };
    (a2) => {
        $crate::Position { rank: 1, file: 0 }
    };
    (a3) => {
        $crate::Position { rank: 2, file: 0 }
    };
    (a4) => {
        $crate::Position { rank: 3, file: 0 }
    };
    (a5) => {
        $crate::Position { rank: 4, file: 0 }
    };
    (a6) => {
        $crate::Position { rank: 5, file: 0 }
    };
    (a7) => {
        $crate::Position { rank: 6, file: 0 }
    };
    (a8) => {
        $crate::Position { rank: 7, file: 0 }
    };
    (b1) => {
        $crate::Position { rank: 0, file: 1 }
    };
    (b2) => {
        $crate::Position { rank: 1, file: 1 }
    };
    (b3) => {
        $crate::Position { rank: 2, file: 1 }
    };
    (b4) => {
        $crate::Position { rank: 3, file: 1 }
    };
    (b5) => {
        $crate::Position { rank: 4, file: 1 }
    };
    (b6) => {
        $crate::Position { rank: 5, file: 1 }
    };
    (b7) => {
        $crate::Position { rank: 6, file: 1 }
    };
    (b8) => {
        $crate::Position { rank: 7, file: 1 }
    };
    (c1) => {
        $crate::Position { rank: 0, file: 2 }
    };
    (c2) => {
        $crate::Position { rank: 1, file: 2 }
    };
    (c3) => {
        $crate::Position { rank: 2, file: 2 }
    };
    (c4) => {
        $crate::Position { rank: 3, file: 2 }
    };
    (c5) => {
        $crate::Position { rank: 4, file: 2 }
    };
    (c6) => {
        $crate::Position { rank: 5, file: 2 }
    };
    (c7) => {
        $crate::Position { rank: 6, file: 2 }
    };
    (c8) => {
        $crate::Position { rank: 7, file: 2 }
    };
    (d1) => {
        $crate::Position { rank: 0, file: 3 }
    };
    (d2) => {
        $crate::Position { rank: 1, file: 3 }
    };
    (d3) => {
        $crate::Position { rank: 2, file: 3 }
    };
    (d4) => {
        $crate::Position { rank: 3, file: 3 }
    };
    (d5) => {
        $crate::Position { rank: 4, file: 3 }
    };
    (d6) => {
        $crate::Position { rank: 5, file: 3 }
    };
    (d7) => {
        $crate::Position { rank: 6, file: 3 }
    };
    (d8) => {
        $crate::Position { rank: 7, file: 3 }
    };
    (e1) => {
        $crate::Position { rank: 0, file: 4 }
    };
    (e2) => {
        $crate::Position { rank: 1, file: 4 }
    };
    (e3) => {
        $crate::Position { rank: 2, file: 4 }
    };
    (e4) => {
        $crate::Position { rank: 3, file: 4 }
    };
    (e5) => {
        $crate::Position { rank: 4, file: 4 }
    };
    (e6) => {
        $crate::Position { rank: 5, file: 4 }
    };
    (e7) => {
        $crate::Position { rank: 6, file: 4 }
    };
    (e8) => {
        $crate::Position { rank: 7, file: 4 }
    };
    (f1) => {
        $crate::Position { rank: 0, file: 5 }
    };
    (f2) => {
        $crate::Position { rank: 1, file: 5 }
    };
    (f3) => {
        $crate::Position { rank: 2, file: 5 }
    };
    (f4) => {
        $crate::Position { rank: 3, file: 5 }
    };
    (f5) => {
        $crate::Position { rank: 4, file: 5 }
    };
    (f6) => {
        $crate::Position { rank: 5, file: 5 }
    };
    (f7) => {
        $crate::Position { rank: 6, file: 5 }
    };
    (f8) => {
        $crate::Position { rank: 7, file: 5 }
    };
    (g1) => {
        $crate::Position { rank: 0, file: 6 }
    };
    (g2) => {
        $crate::Position { rank: 1, file: 6 }
    };
    (g3) => {
        $crate::Position { rank: 2, file: 6 }
    };
    (g4) => {
        $crate::Position { rank: 3, file: 6 }
    };
    (g5) => {
        $crate::Position { rank: 4, file: 6 }
    };
    (g6) => {
        $crate::Position { rank: 5, file: 6 }
    };
    (g7) => {
        $crate::Position { rank: 6, file: 6 }
    };
    (g8) => {
        $crate::Position { rank: 7, file: 6 }
    };
    (h1) => {
        $crate::Position { rank: 0, file: 7 }
    };
    (h2) => {
        $crate::Position { rank: 1, file: 7 }
    };
    (h3) => {
        $crate::Position { rank: 2, file: 7 }
    };
    (h4) => {
        $crate::Position { rank: 3, file: 7 }
    };
    (h5) => {
        $crate::Position { rank: 4, file: 7 }
    };
    (h6) => {
        $crate::Position { rank: 5, file: 7 }
    };
    (h7) => {
        $crate::Position { rank: 6, file: 7 }
    };
    (h8) => {
        $crate::Position { rank: 7, file: 7 }
    };
}

#[macro_export]
macro_rules! positions {
    ($($pos:ident),+) => {
        {
            [
                $($crate::position!($pos)),+
            ]
        }
    };
}

const DEFAULT: Game = Game {
    white_pieces: PieceSet([
        BitBoard::from_positions(positions!(e1)),
        BitBoard::from_positions(positions!(d1)),
        BitBoard::from_positions(positions!(a1, h1)),
        BitBoard::from_positions(positions!(c1, f1)),
        BitBoard::from_positions(positions!(b1, g1)),
        BitBoard::from_positions(positions!(a2, b2, c2, d2, e2, f2, g2, h2)),
    ]),
    black_pieces: PieceSet([
        BitBoard::from_positions(positions!(e8)),
        BitBoard::from_positions(positions!(d8)),
        BitBoard::from_positions(positions!(a8, h8)),
        BitBoard::from_positions(positions!(c8, f8)),
        BitBoard::from_positions(positions!(b8, g8)),
        BitBoard::from_positions(positions!(a7, b7, c7, d7, e7, f7, g7, h7)),
    ]),
    current_player: Colour::White,
    castling_rights: CastlingRights {
        white_king: true,
        white_queen: true,
        black_king: true,
        black_queen: true,
    },
    en_passant: None,
    half_moves: 0,
    full_moves: 1,
    history: vec![],
    captured_pieces: vec![],
};

#[derive(Debug, Error)]
pub enum FenParseError {
    #[error("Wrong length")]
    BadLen,
    #[error("Failed to parse piece")]
    BadPieces(#[from] PiecesParseError),
    #[error("unrecognised colour")]
    UnrecognisedColour,
    #[error("failed to parse en passant position")]
    EnPassantParseError(#[from] PositionParseError),
    #[error("failed to parse move")]
    MoveParseError(#[from] ParseIntError),
    #[error("King not found")]
    KingNotFound,
}

impl FromStr for Game {
    type Err = FenParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split(' ');
        let mut white_pieces = [BitBoard::default(); 6];
        let mut black_pieces = [BitBoard::default(); 6];
        let pieces = parts.next().ok_or(PiecesParseError::NotFound)?;
        let mut ranks = pieces.split('/');
        let mut current_rank: i8 = 7;
        while current_rank >= 0 {
            let rank = ranks.next().ok_or(PiecesParseError::MissingRanks)?;
            let mut files = rank.chars();
            let mut current_file: i8 = 0;
            while current_file < 8 {
                let char = files.next().ok_or(PiecesParseError::MissingFiles)?;
                if let Some(step) = char.to_digit(10) {
                    current_file += step as i8;
                    continue;
                }
                let piece_type = match char
                    .to_lowercase()
                    .next()
                    .ok_or(PiecesParseError::UnrecognisedPieceType)?
                {
                    'r' => PieceType::Rook,
                    'n' => PieceType::Knight,
                    'b' => PieceType::Bishop,
                    'q' => PieceType::Queen,
                    'k' => PieceType::King,
                    'p' => PieceType::Pawn,
                    _ => return Err(PiecesParseError::UnrecognisedPieceType.into()),
                };
                let piece_arrs = if char.is_uppercase() {
                    &mut white_pieces
                } else {
                    &mut black_pieces
                };
                piece_arrs[piece_type as usize] += Position {
                    rank: current_rank,
                    file: current_file,
                };
                current_file += 1;
            }
            current_rank -= 1
        }
        let next_turn = match parts.next().ok_or(FenParseError::BadLen)? {
            "w" => Colour::White,
            "b" => Colour::Black,
            _ => return Err(FenParseError::UnrecognisedColour),
        };
        let castling_part = parts.next().ok_or(FenParseError::BadLen)?;
        let castling_rights = CastlingRights {
            white_king: castling_part.contains('K'),
            white_queen: castling_part.contains('Q'),
            black_king: castling_part.contains('k'),
            black_queen: castling_part.contains('q'),
        };

        let en_passant_part = parts.next().ok_or(FenParseError::BadLen)?;
        let en_passant: Option<Position> = if en_passant_part == "-" {
            None
        } else {
            Some(en_passant_part.parse()?)
        };

        let mut half_moves: u32 = 0;
        let mut full_moves: u32 = 1;

        for moves in [&mut half_moves, &mut full_moves] {
            if let Some(next_part) = parts.next() {
                if !next_part.is_empty() {
                    *moves = next_part.parse()?
                }
            }
        }

        Ok(Game {
            white_pieces: white_pieces
                .try_into()
                .ok()
                .ok_or(FenParseError::KingNotFound)?,
            black_pieces: black_pieces
                .try_into()
                .ok()
                .ok_or(FenParseError::KingNotFound)?,
            current_player: next_turn,
            castling_rights,
            en_passant,
            full_moves,
            half_moves,
            history: vec![],
            captured_pieces: vec![],
        })
    }
}

#[cfg(test)]
mod test {
    use crate::{CastlingRights, Colour, Game, Position};

    #[test]
    fn from_str() {
        let game: Game = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - "
            .parse()
            .expect("failed to parse game");
        assert_eq!(
            game,
            Game {
                white_pieces: [
                    Position { rank: 0, file: 4 }.into(),
                    Position { rank: 2, file: 5 }.into(),
                    vec!((0, 0), (0, 7)).into_iter().collect(),
                    vec!((1, 3), (1, 4)).into_iter().collect(),
                    vec!((2, 2), (4, 4)).into_iter().collect(),
                    vec!(
                        (1, 0),
                        (1, 1),
                        (1, 2),
                        (4, 3),
                        (3, 4),
                        (1, 5),
                        (1, 6),
                        (1, 7)
                    )
                    .into_iter()
                    .collect(),
                ]
                .try_into()
                .expect("king is present"),
                black_pieces: [
                    Position { rank: 7, file: 4 }.into(),
                    vec!((6, 4)).into_iter().collect(),
                    vec!((7, 0), (7, 7)).into_iter().collect(),
                    vec!((5, 0), (6, 6)).into_iter().collect(),
                    vec!((5, 1), (5, 5)).into_iter().collect(),
                    vec![
                        (6, 0),
                        (3, 1),
                        (6, 2),
                        (6, 3),
                        (5, 4),
                        (6, 5),
                        (5, 6),
                        (2, 7),
                    ]
                    .into_iter()
                    .collect(),
                ]
                .try_into()
                .expect("king is present"),
                current_player: Colour::White,
                castling_rights: CastlingRights {
                    white_king: true,
                    white_queen: true,
                    black_king: true,
                    black_queen: true,
                },
                en_passant: None,
                half_moves: 0,
                full_moves: 1,
                history: vec![],
                captured_pieces: vec![],
            }
        )
    }

    #[test]
    fn default() {
        assert_eq!(Game::default().to_string(),
                   r#"_________________
|♖|♘|♗|♕|♔|♗|♘|♖|
_________________
|♙|♙|♙|♙|♙|♙|♙|♙|
_________________
| | | | | | | | |
_________________
| | | | | | | | |
_________________
| | | | | | | | |
_________________
| | | | | | | | |
_________________
|♟|♟|♟|♟|♟|♟|♟|♟|
_________________
|♜|♞|♝|♛|♚|♝|♞|♜|
_________________
"#)
    }
}
