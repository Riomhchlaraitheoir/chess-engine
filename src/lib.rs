mod map_while_inclusive;
mod models;
#[cfg(test)]
mod test;

use std::iter::empty;
use std::ops::ControlFlow;
use itertools::chain;
use thiserror::Error;
use log::debug;
pub use crate::models::*;

const KING_MOVES: [BitBoard; 64] = {
    let mut arr = [BitBoard::const_default(); 64];
    let mut i: usize = 0;
    while i < 64 {
        let pos = Position::from_index(i);
        let rank = pos.rank;
        let file = pos.file;
        let mut board = 0u64;

        let directions: [(i8, i8); 8] = [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ];
        let mut j = 0;
        while j < 8 {
            let (d_rank, d_file) = directions[j];
            let rank = rank + d_rank;
            let file = file + d_file;
            if 0 <= file && file < 8 && 0 <= rank && rank < 8 {
                board |= 1u64 << Position { rank, file }.index();
            }
            j += 1;
        }
        arr[i] = BitBoard::const_from(board);
        i += 1;
    }
    arr
};

const KNIGHT_MOVES: [BitBoard; 64] = {
    let mut arr = [BitBoard::const_default(); 64];
    let mut i: usize = 0;
    while i < 64 {
        let pos = Position::from_index(i);
        let rank = pos.rank;
        let file = pos.file;
        let mut board = 0u64;

        let directions: [(i8, i8); 8] = [
            (-1, -2),
            (-2, -1),
            (1, -2),
            (-2, 1),
            (-1, 2),
            (2, -1),
            (1, 2),
            (2, 1),
        ];
        let mut j = 0;
        while j < 8 {
            let (d_rank, d_file) = directions[j];
            let rank = rank + d_rank;
            let file = file + d_file;
            if 0 <= file && file < 8 && 0 <= rank && rank < 8 {
                board |= 1u64 << Position { rank, file }.index();
            }
            j += 1;
        }
        arr[i] = BitBoard::const_from(board);
        i += 1;
    }
    arr
};

#[derive(Debug, Error)]
pub enum IllegalMoveError {
    #[error("Piece not found")]
    PieceNotFound,
    #[error("No piece was found to capture")]
    CapturedPieceNotFound,
}

impl Game {
    pub fn make_move(&mut self, m: Move) -> Result<Vec<BoardUpdate>, IllegalMoveError> {
        let from: BitBoard = m.from.into();
        let to: BitBoard = m.to.into();
        let (_, from_type) = self.piece_at(from).ok_or(IllegalMoveError::PieceNotFound)?;
        let to_type = m.move_type.promote_to().unwrap_or(from_type);
        self.assert_is_legal(m)?;
        let old_castling_rights = self.castling_rights;
        let first_rank: i8 = match self.current_player {
            Colour::White => 0,
            Colour::Black => 7,
        };
        let mut updates = Vec::new();
        {
            // check for castling rights change
            let lost_rights = match from_type {
                PieceType::King => (true, true),
                PieceType::Rook => {
                    if m.from.rank == first_rank {
                        match m.from.file {
                            0 => (true, false),
                            7 => (false, true),
                            _ => (false, false),
                        }
                    } else {
                        (false, false)
                    }
                }
                _ => (false, false),
            };
            match self.current_player {
                Colour::White => {
                    if lost_rights.1 {
                        self.castling_rights.white_king = false;
                    }
                    if lost_rights.0 {
                        self.castling_rights.white_queen = false;
                    }
                }
                Colour::Black => {
                    if lost_rights.1 {
                        self.castling_rights.black_king = false;
                    }
                    if lost_rights.0 {
                        self.castling_rights.black_queen = false;
                    }
                }
            };
        };

        {
            // update bit boards
            let our_pieces = match self.current_player {
                Colour::White => &mut self.white_pieces,
                Colour::Black => &mut self.black_pieces,
            };
            our_pieces[from_type] ^= from;
            our_pieces[to_type] ^= to;
            updates.push(BoardUpdate::Move {
                from: m.from,
                to: m.to,
                new_type: to_type,
            })
        }
        {
            // remove any captured pieces
            let captured_pos = m.captured_position();
            if let Some(captured_pos) = captured_pos {
                let captured_type = match self.current_player {
                    Colour::White => self.black_piece_at(captured_pos),
                    Colour::Black => self.white_piece_at(captured_pos),
                }
                .ok_or(IllegalMoveError::CapturedPieceNotFound)?;
                let enemy_pieces = match self.current_player {
                    Colour::White => &mut self.black_pieces,
                    Colour::Black => &mut self.white_pieces,
                };
                enemy_pieces.remove_from(captured_pos);
                self.captured_pieces.push(captured_type);
                if captured_type == PieceType::Rook && captured_pos.rank == 7 - first_rank {
                    match (!self.current_player, captured_pos.file) {
                        (Colour::White, 0) => self.castling_rights.white_queen = false,
                        (Colour::White, 7) => self.castling_rights.white_king = false,
                        (Colour::Black, 0) => self.castling_rights.black_queen = false,
                        (Colour::Black, 7) => self.castling_rights.black_king = false,
                        _ => {}
                    }
                }
                updates.push(BoardUpdate::Remove(captured_pos))
            }
        };
        {
            let our_pieces = match self.current_player {
                Colour::White => &mut self.white_pieces,
                Colour::Black => &mut self.black_pieces,
            };
            let first_rank = (self.current_player as i8) * 7;
            match m.move_type {
                MoveType::QueenSideCastle => {
                    let from = Position {
                        rank: first_rank,
                        file: 0,
                    };
                    let to = Position {
                        rank: first_rank,
                        file: 3,
                    };
                    our_pieces[PieceType::Rook].toggle(from);
                    our_pieces[PieceType::Rook].toggle(to);
                    updates.push(BoardUpdate::Move {
                        from,
                        to,
                        new_type: PieceType::Rook,
                    })
                }
                MoveType::KingSideCastle => {
                    let from = Position {
                        rank: first_rank,
                        file: 7,
                    };
                    let to = Position {
                        rank: first_rank,
                        file: 5,
                    };
                    our_pieces[PieceType::Rook].toggle(from);
                    our_pieces[PieceType::Rook].toggle(to);
                    updates.push(BoardUpdate::Move {
                        from,
                        to,
                        new_type: PieceType::Rook,
                    })
                }
                _ => {}
            }
        };

        self.en_passant = if m.move_type == MoveType::DoublePawnPush {
            Some(Position {
                rank: 2 + (self.current_player as i8) * 3,
                file: m.from.file,
            })
        } else {
            None
        };
        self.current_player = !self.current_player;
        let old_castling_rights = if old_castling_rights == self.castling_rights {
            None
        } else {
            Some(old_castling_rights)
        };
        self.history.push((m, old_castling_rights));

        Ok(updates)
    }

    pub fn pieces(&self) -> impl Iterator<Item = Piece> + '_ {
        [
            (Colour::White, &self.white_pieces),
            (Colour::Black, &self.black_pieces),
        ]
        .into_iter()
        .flat_map(|(colour, pieces)| {
            PieceType::ALL.into_iter().flat_map(move |piece_type| {
                let bitboard = pieces[piece_type];
                bitboard.iter().map(move |position| Piece {
                    colour,
                    piece_type,
                    position,
                })
            })
        })
    }

    pub(crate) fn build_array(&self) -> [Option<(Colour, PieceType)>; 64] {
        let mut array: [Option<(Colour, PieceType)>; 64] = [None; 64];
        for (colour, pieces) in [
            (Colour::White, &self.white_pieces),
            (Colour::Black, &self.black_pieces),
        ] {
            for i in 0..6 {
                let kind: PieceType = i.into();
                let bitboard = pieces[kind];
                for pos in bitboard.iter() {
                    array[pos.index()] = Some((colour, kind))
                }
            }
        }
        array
    }

    fn our_pieces(&self) -> &PieceSet {
        match self.current_player {
            Colour::White => &self.white_pieces,
            Colour::Black => &self.black_pieces,
        }
    }

    fn all_pieces(&self) -> BitBoard {
        self.white_pieces
            .iter()
            .chain(self.black_pieces.iter())
            .map(|(_, b)| b)
            .collect()
    }

    fn all_our_pieces(&self) -> BitBoard {
        self.our_pieces().iter().map(|(_, b)| b).collect()
    }

    fn enemy_pieces(&self) -> &PieceSet {
        match self.current_player {
            Colour::White => &self.black_pieces,
            Colour::Black => &self.white_pieces,
        }
    }

    fn all_enemy_pieces(&self) -> BitBoard {
        self.enemy_pieces().iter().map(|(_, b)| b).collect()
    }

    pub fn get_legal_moves(&self, from: Option<Position>) -> Vec<Move> {
        let pin_enforcer = self.pin_enforcer();
        let check_enforcer = self.check_enforcer();
        self.get_pseudo_legal_moves(from)
            .filter(|m| {
                self.enforce_pin(&pin_enforcer, m)
                    && (check_enforcer
                        .as_ref()
                        .is_none_or(|check_enforcer| self.enforce_check(check_enforcer, m)))
            })
            .filter(|m| {
                // filter special case: en passant pins
                if m.move_type != MoveType::EnPassant {
                    return true;
                }
                let our_king = self.our_pieces().king();
                let king_direction = Direction::between(&m.from, &our_king);
                let king_direction = match king_direction {
                    Some(Direction::LEFT) => Direction::LEFT,
                    Some(Direction::RIGHT) => Direction::RIGHT,
                    _ => return true,
                };
                #[allow(clippy::expect_used)]
                let captured = m
                    .captured_position()
                    .expect("en passant moves must capture something");

                // something else is blocking the king, all is well
                if self
                    .next_piece_in_direction(&m.from, &king_direction, Some(captured))
                    .map(|p| p.position)
                    != Some(our_king)
                {
                    return true;
                }
                let attacker_direction = -king_direction;
                match self.next_piece_in_direction(
                    &m.from,
                    &attacker_direction,
                    Some(Position {
                        rank: m.from.rank,
                        file: m.to.file,
                    }),
                ) {
                    Some(attacker) => {
                        if attacker.colour == self.current_player {
                            return true;
                        }

                        !matches!(attacker.piece_type, PieceType::Queen | PieceType::Rook)
                    }
                    None => true,
                }
            })
            .collect()
    }

    fn check_enforcer(&self) -> Option<CheckEnforcer> {
        if !matches!(self.status(), Status::Check { .. }) {
            return None;
        }
        #[allow(clippy::expect_used)]
        let our_king = self.our_pieces()[PieceType::King]
            .iter()
            .next()
            .expect("No King found!!");
        let attackers: Vec<_> = self
            .get_attackers(our_king, None)
            .filter(|p| p.colour != self.current_player)
            .map(|attacker| {
                let can_block = matches!(
                    attacker.piece_type,
                    PieceType::Queen | PieceType::Bishop | PieceType::Rook
                );
                let attack_direction = if can_block {
                    Direction::between(&attacker.position, &our_king)
                } else {
                    None
                };
                (attacker.position, attack_direction)
            })
            .collect();

        Some(CheckEnforcer {
            our_king,
            attackers,
        })
    }

    fn enforce_check(
        &self,
        CheckEnforcer {
            our_king,
            attackers,
        }: &CheckEnforcer,
        m: &Move,
    ) -> bool {
        // ensure that the current check is dealt with
        // Possible solutions:
        // 1. Move the king out of check
        // 2. Capture the attacking piece
        // 3. Block the attack

        // if the king is moving, this move is legal as the move has already
        // been tested to assure the king is not moving into check
        if m.from == *our_king {
            return true;
        }
        attackers
            .iter()
            .cloned()
            .all(|(attacker, attack_direction)| {
                // if capturing the attacker, this move is legal
                if m.captured_position() == Some(attacker) {
                    return true;
                }

                match attack_direction {
                    None => {
                        // if the attack cannot be blocked, this move cannot be legal
                        false
                    }
                    Some(dir) => {
                        dir.is_between(&attacker, &m.to) && dir.is_between(&m.to, our_king)
                    }
                }
            })
    }

    fn enforce_pin(&self, pe: &(Position, Vec<(Position, Direction)>), m: &Move) -> bool {
        let (our_king, pinned) = pe;
        let Move {
            from,
            to,
            move_type: _,
        } = m;
        if from == our_king {
            return !self.is_under_enemy_attack(*to, Some(*our_king));
        }
        match pinned.iter().find(|(pos, _)| pos == from) {
            None => true, // if the moving piece is not pinned, it is legal
            Some((_, direction)) => {
                // if the move is still blocking the attack, it is legal
                direction.is_between(our_king, to)
            }
        }
    }

    fn is_under_enemy_attack(&self, to: Position, ignore_position: Option<Position>) -> bool {
        let attacker = self
            .get_attackers(to, ignore_position)
            .find(|p| p.colour != self.current_player);

        attacker.is_some()
    }

    // get_attackers: returns an iterator for all pieces which can attack the given position, ignoring theany piece in the other position
    fn get_attackers<'a>(
        &'a self,
        position: Position,
        ignore_position: Option<Position>,
    ) -> Box<dyn Iterator<Item = Piece> + 'a> {
        let types: [Box<dyn Iterator<Item = Piece>>; 5] = [
            // get king attackers
            Box::new({
                [
                    (Colour::White, &self.white_pieces),
                    (Colour::Black, &self.black_pieces),
                ]
                .into_iter()
                .flat_map(move |(colour, pieces)| {
                    (KING_MOVES[position.index()] & pieces[PieceType::King])
                        .iter()
                        .map(move |position| Piece {
                            piece_type: PieceType::King,
                            position,
                            colour,
                        })
                })
            }),
            // get knight attackers
            Box::new({
                [
                    (Colour::White, &self.white_pieces),
                    (Colour::Black, &self.black_pieces),
                ]
                .into_iter()
                .flat_map(move |(colour, pieces)| {
                    (KNIGHT_MOVES[position.index()] & pieces[PieceType::Knight])
                        .iter()
                        .map(move |position| Piece {
                            piece_type: PieceType::Knight,
                            position,
                            colour,
                        })
                })
            }),
            // get pawn attackers
            Box::new({
                Direction::DIAGONALS.iter().filter_map(move |direction| {
                    let pawn_position = direction.step(&position)?;
                    let needed_colour = if pawn_position.rank < position.rank {
                        Colour::White
                    } else {
                        Colour::Black
                    };
                    let pawn = self.piece_at(pawn_position)?;
                    if pawn == (needed_colour, PieceType::Pawn) {
                        Some(Piece {
                            colour: pawn.0,
                            piece_type: pawn.1,
                            position: pawn_position,
                        })
                    } else {
                        None
                    }
                })
            }),
            // get diagonal attackers
            Box::new({
                Direction::DIAGONALS
                    .iter()
                    .filter_map(move |direction| -> Option<Piece> {
                        let attacker =
                            self.next_piece_in_direction(&position, direction, ignore_position)?;

                        if attacker.piece_type != PieceType::Bishop
                            && attacker.piece_type != PieceType::Queen
                        {
                            None
                        } else {
                            Some(attacker)
                        }
                    })
            }),
            // get non-diagonal attackers
            Box::new({
                Direction::NON_DIAGONAL
                    .iter()
                    .filter_map(move |direction| -> Option<Piece> {
                        let attacker =
                            self.next_piece_in_direction(&position, direction, ignore_position)?;

                        if attacker.piece_type != PieceType::Rook
                            && attacker.piece_type != PieceType::Queen
                        {
                            None
                        } else {
                            Some(attacker)
                        }
                    })
            }),
        ];
        Box::new(types.into_iter().flatten())
    }

    fn pin_enforcer(&self) -> (Position, Vec<(Position, Direction)>) {
        let our_king = self.our_pieces().king();
        let pinned: Vec<_> = Direction::ALL
            .iter()
            .copied()
            .filter_map(|dir| -> Option<(Position, Direction)> {
                // get a piece that could be pinned
                let pinned = self.next_piece_in_direction(&our_king, &dir, None)?;
                // if this is an enemy piece, it cannot be pinned
                if pinned.colour != self.current_player {
                    return None;
                }

                // get a piece that could be pinning the pinned Piece above
                let attacker = self.next_piece_in_direction(&pinned.position, &dir, None)?;

                // if the attacker is our piece, it cannot pin (pinned: Piece) above
                if attacker.colour == self.current_player {
                    return None;
                }

                let can_pin = match attacker.piece_type {
                    PieceType::King => false, // cannot pin any piece, as it cannot move far enough
                    PieceType::Queen => true, // will always be a pin at this point
                    PieceType::Rook => !dir.is_diagonal(), // will be a pin iff the direction of attack is not diagonal
                    PieceType::Bishop => dir.is_diagonal(), // will be a pin iff the direction of attack is diagonal
                    PieceType::Knight => false, // cannot pin any piece, due to the nature of knight movement
                    PieceType::Pawn => false, // cannot pin any piece, as it cannot move far enough
                };
                if !can_pin {
                    return None;
                }

                Some((pinned.position, dir))
            })
            .collect();
        (our_king, pinned)
    }

    fn next_piece_in_direction(
        &self,
        from: &Position,
        direction: &Direction,
        ignore_position: Option<Position>,
    ) -> Option<Piece> {
        let iter = LineIterator {
            current: *from,
            direction: *direction,
        };
        let (position, colour, piece_type) = iter
            .filter_map(|p| self.piece_at(p).map(|(c, t)| (p, c, t)))
            .find(|(p, _, _)| ignore_position != Some(*p))?;
        Some(Piece {
            colour,
            piece_type,
            position,
        })
    }

    pub fn get_pseudo_legal_moves(
        &self,
        from: Option<Position>,
    ) -> Box<dyn Iterator<Item = Move> + '_> {
        let pieces = self.our_pieces();

        if let Some(from) = from {
            let Some((colour, kind)) = self.piece_at(from) else {
                return Box::new(empty());
            };
            if colour != self.current_player {
                return Box::new(empty());
            }
            return match kind {
                PieceType::King => Box::new(self.get_king_moves(from)),
                PieceType::Queen => Box::new(self.get_queen_moves(from)),
                PieceType::Rook => Box::new(self.get_rook_moves(from)),
                PieceType::Bishop => Box::new(self.get_bishop_moves(from)),
                PieceType::Knight => Box::new(self.get_knight_moves(from)),
                PieceType::Pawn => Box::new(self.get_pawn_moves(from)),
            };
        }

        let king_moves = pieces[PieceType::King]
            .iter()
            .flat_map(|from| self.get_king_moves(from));
        let queen_moves = pieces[PieceType::Queen]
            .iter()
            .flat_map(|from| self.get_queen_moves(from));
        let rook_moves = pieces[PieceType::Rook]
            .iter()
            .flat_map(|from| self.get_rook_moves(from));
        let bishop_moves = pieces[PieceType::Bishop]
            .iter()
            .flat_map(|from| self.get_bishop_moves(from));
        let knight_moves = pieces[PieceType::Knight]
            .iter()
            .flat_map(|from| self.get_knight_moves(from));
        let pawn_moves = pieces[PieceType::Pawn]
            .iter()
            .flat_map(|from| self.get_pawn_moves(from));
        Box::new(
            chain!(
                king_moves,
                queen_moves,
                rook_moves,
                bishop_moves,
                knight_moves,
                pawn_moves,
            )
        )
    }

    fn get_king_moves(&self, from: Position) -> impl Iterator<Item = Move> {
        self.get_king_moves_vec(from).into_iter()
    }

    fn get_king_moves_vec(&self, from: Position) -> Vec<Move> {
        let possible = KING_MOVES[from.index()] & !self.all_our_pieces();
        let mut moves: Vec<Move> = possible
            .iter()
            .map(|to| Move {
                from,
                to,
                move_type: {
                    if self.all_enemy_pieces().contains(to) {
                        MoveType::Capture
                    } else {
                        MoveType::Normal
                    }
                },
            })
            .collect();

        if self.status() != Status::Normal {
            return moves;
        }

        if self.castling_rights.queen_side(self.current_player)
            && self
                .next_piece_in_direction(&from, &Direction::LEFT, None)
                .is_some_and(|p| p.position.file == 0)
            && !self.is_under_enemy_attack(Position { file: 3, ..from }, None)
            && !self.is_under_enemy_attack(Position { file: 2, ..from }, None)
        {
            moves.push(Move {
                from,
                to: Position { file: 2, ..from },
                move_type: MoveType::QueenSideCastle,
            });
        }

        if self.castling_rights.king_side(self.current_player) {
            if self
                .next_piece_in_direction(&from, &Direction::RIGHT, None)
                .is_some_and(|p| p.position.file == 7)
            {
                if !self.is_under_enemy_attack(Position { file: 5, ..from }, None) {
                    if !self.is_under_enemy_attack(Position { file: 6, ..from }, None) {
                        moves.push(Move {
                            from,
                            to: Position { file: 6, ..from },
                            move_type: MoveType::KingSideCastle,
                        });
                    } else {
                        debug!("file 6 under threat")
                    }
                } else {
                    debug!("file 5 under threat")
                }
            } else {
                debug!("next piece is not at the last file")
            }
        } else {
            debug!("no right to castle")
        }

        moves
    }

    fn get_queen_moves(&self, from: Position) -> impl Iterator<Item = Move> {
        self.get_queen_moves_vec(from).into_iter()
    }

    fn get_queen_moves_vec(&self, from: Position) -> Vec<Move> {
        Direction::ALL
            .iter()
            .flat_map(|dir| self.get_moves_in_direction(from, dir))
            .collect()
    }

    fn get_bishop_moves(&self, from: Position) -> impl Iterator<Item = Move> {
        self.get_bishop_moves_vec(from).into_iter()
    }

    fn get_bishop_moves_vec(&self, from: Position) -> Vec<Move> {
        Direction::DIAGONALS
            .iter()
            .flat_map(|dir| self.get_moves_in_direction(from, dir))
            .collect()
    }

    fn get_knight_moves(&self, from: Position) -> impl Iterator<Item = Move> {
        let possible = KNIGHT_MOVES[from.index()] & !self.all_our_pieces();
        possible
            .iter()
            .map(move |to| Move {
                from,
                to,
                move_type: {
                    if self.all_enemy_pieces().contains(to) {
                        MoveType::Capture
                    } else {
                        MoveType::Normal
                    }
                },
            })
    }

    fn get_rook_moves(&self, from: Position) -> impl Iterator<Item = Move> {
        self.get_rook_moves_vec(from).into_iter()
    }

    fn get_rook_moves_vec(&self, from: Position) -> Vec<Move> {
        Direction::NON_DIAGONAL
            .iter()
            .flat_map(|dir| self.get_moves_in_direction(from, dir))
            .collect()
    }

    fn get_pawn_moves(&self, from: Position) -> impl Iterator<Item = Move> {
        self.get_pawn_moves_vec(from).into_iter()
    }

    fn get_pawn_moves_vec(&self, from: Position) -> Vec<Move> {
        let d_rank_forward: i8;
        let starting_rank: i8;
        let last_rank: i8;
        match self.current_player {
            Colour::White => {
                d_rank_forward = 1;
                starting_rank = 1;
                last_rank = 7;
            }
            Colour::Black => {
                d_rank_forward = -1;
                starting_rank = 6;
                last_rank = 0;
            }
        }
        let mut position = from;
        let mut on_second_rank = false;
        let mut moves = vec![];
        loop {
            position.rank += d_rank_forward;
            if position.out_of_bounds() {
                break;
            }

            let to: BitBoard = position.into();
            // if the target square is occupied
            if !(to & self.all_pieces()).is_empty() {
                break;
            }
            if position.rank == last_rank {
                moves.push(Move {
                    from,
                    to: position,
                    move_type: MoveType::KnightPromotion,
                });
                moves.push(Move {
                    from,
                    to: position,
                    move_type: MoveType::BishopPromotion,
                });
                moves.push(Move {
                    from,
                    to: position,
                    move_type: MoveType::RookPromotion,
                });
                moves.push(Move {
                    from,
                    to: position,
                    move_type: MoveType::QueenPromotion,
                });
            } else {
                moves.push(Move {
                    from,
                    to: position,
                    move_type: {
                        if on_second_rank {
                            MoveType::DoublePawnPush
                        } else {
                            MoveType::Normal
                        }
                    },
                });
            }
            if from.rank != starting_rank || on_second_rank {
                break;
            }
            on_second_rank = true;
        }
        for d_file in [-1, 1] {
            let position = Position {
                rank: from.rank + d_rank_forward,
                file: from.file + d_file,
            };
            if position.out_of_bounds() {
                continue;
            }
            let to: BitBoard = position.into();
            if !(to & self.all_enemy_pieces()).is_empty()
                || self.en_passant.is_some_and(|p| p == position)
            {
                if position.rank == last_rank {
                    moves.push(Move {
                        from,
                        to: position,
                        move_type: MoveType::KnightPromotionCapture,
                    });
                    moves.push(Move {
                        from,
                        to: position,
                        move_type: MoveType::BishopPromotionCapture,
                    });
                    moves.push(Move {
                        from,
                        to: position,
                        move_type: MoveType::RookPromotionCapture,
                    });
                    moves.push(Move {
                        from,
                        to: position,
                        move_type: MoveType::QueenPromotionCapture,
                    });
                } else {
                    moves.push(Move {
                        from,
                        to: position,
                        move_type: {
                            if self.en_passant.is_some_and(|p| p == position) {
                                MoveType::EnPassant
                            } else {
                                MoveType::Capture
                            }
                        },
                    });
                }
            }
        }
        moves
    }

    fn get_moves_in_direction(
        &self,
        from: Position,
        direction: &Direction,
    ) -> impl Iterator<Item = Move> + '_ {
        let line = LineIterator {
            current: from,
            direction: *direction,
        };
        use crate::map_while_inclusive::IterExt;
        line.map_while_inclusive(move |position| {
            let to: BitBoard = position.into();
            if !(to & self.all_our_pieces()).is_empty() {
                ControlFlow::Break(None)
            } else if !(to & self.all_enemy_pieces()).is_empty() {
                ControlFlow::Break(Some(Move {
                    from,
                    to: position,
                    move_type: MoveType::Capture,
                }))
            } else {
                ControlFlow::Continue(Move {
                    from,
                    to: position,
                    move_type: MoveType::Normal,
                })
            }
        })
    }

    pub fn status(&self) -> Status {
        let our_king = self.our_pieces().king();
        let attacker = self
            .get_attackers(our_king, None)
            .find(|p| p.colour != self.current_player);
        if let Some(attacker) = attacker {
            Status::Check {
                colour: self.current_player,
                attacker: attacker.position,
            } // TODO handle CheckMate
        } else {
            Status::Normal
        }
    }

    fn piece_at(&self, from: impl Into<BitBoard>) -> Option<(Colour, PieceType)> {
        let from = from.into();
        if let Some(kind) = self.white_piece_at(from) {
            Some((Colour::White, kind))
        } else {
            self.black_piece_at(from).map(|kind| (Colour::Black, kind))
        }
    }

    fn white_piece_at(&self, from: impl Into<BitBoard>) -> Option<PieceType> {
        let from = from.into();
        for (kind, board) in self.white_pieces.iter() {
            if !(from & board).is_empty() {
                return Some(kind);
            }
        }
        None
    }

    fn black_piece_at(&self, from: impl Into<BitBoard>) -> Option<PieceType> {
        let from = from.into();
        for (kind, board) in self.black_pieces.iter() {
            if !(from & board).is_empty() {
                return Some(kind);
            }
        }
        None
    }

    fn assert_is_legal(&self, _: Move) -> Result<(), IllegalMoveError> {
        // TODO: implement
        Ok(())
    }
}

struct CheckEnforcer {
    our_king: Position,
    attackers: Vec<(Position, Option<Direction>)>,
}

#[derive(Debug, Error)]
pub enum PiecesParseError {
    #[error("Not found")]
    NotFound,
    #[error("Missing ranks")]
    MissingRanks,
    #[error("Missing files")]
    MissingFiles,
    #[error("Unrecognised piece type")]
    UnrecognisedPieceType,
}

struct LineIterator {
    current: Position,
    direction: Direction,
}

impl Iterator for LineIterator {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.direction.step(&self.current)?;
        self.current = next;
        Some(next)
    }
}

impl FromIterator<BitBoard> for Vec<Position> {
    fn from_iter<T: IntoIterator<Item = BitBoard>>(iter: T) -> Self {
        let or_all = iter.into_iter().reduce(|a, b| a | b);
        match or_all {
            Some(b) => b.iter().collect(),
            None => vec![],
        }
    }
}
