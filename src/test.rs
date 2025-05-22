use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    io::Write,
    process::{Command, Stdio},
    str::FromStr,
};

use itertools::Itertools;
use thiserror::Error;
use crate::{
    BoardUpdate, MoveType,
    models::{CastlingRights, Colour, Move, PieceType, Position, PositionParseError},
};

use super::Game;

#[test]
fn default_board() {
    let board = Game::default();
    assert_eq!(
        board
            .white_pieces
            .iter()
            .flat_map(|(_, b)| b.iter().collect::<Vec<_>>())
            .collect::<Vec<_>>()
            .len(),
        16
    );
    assert_eq!(
        board
            .black_pieces
            .iter()
            .flat_map(|(_, b)| b.iter().collect::<Vec<_>>())
            .collect::<Vec<_>>()
            .len(),
        16
    );
    assert!(board.castling_rights.white_king);
    assert!(board.castling_rights.white_queen);
    assert!(board.castling_rights.black_king);
    assert!(board.castling_rights.black_queen);
}

fn test_perft(case_num: usize, depth: usize) {
    let PerftCase { fen, values }: PerftCase = CASES[case_num];
    let expected = &values[0..depth];
    let board: Game = fen
        .map(|s| s.parse().expect("Should parse"))
        .unwrap_or_default();
    let mut values = vec![0u64; depth];
    perft_depths(&board, &mut values, depth);
    for i in 0..depth {
        assert_eq!(expected[i], values[depth - i - 1]);
    }
}

fn perft_depths(board: &Game, values: &mut [u64], depth: usize) {
    let moves = board.get_legal_moves(None);
    if depth == 0 {
        return;
    }
    values[depth - 1] += moves.len() as u64;
    for m in moves {
        // println!("{m}");
        let mut child = board.clone();
        child.make_move(m).expect("Move should be legal");
        perft_depths(&child, values, depth - 1);
        // print!("\x1b[2K")
    }
}

impl TryFrom<Option<&str>> for Game {
    type Error = <Self as FromStr>::Err;
    fn try_from(value: Option<&str>) -> Result<Self, Self::Error> {
        match value {
            Some(fen) => Self::from_str(fen),
            None => Ok(Self::default()),
        }
    }
}

fn debug_perft<'a>(
    depth: u8,
    fen: Option<&'a str>,
    path: &'a [MoveDesc],
) -> Result<(), Vec<MoveDesc>> {
    let mut board = fen.try_into().expect("failed to parse fen");
    make_moves(&mut board, path);
    let actual = children_perft(board, depth);
    let expected = stockfish(depth, &fen, path.to_vec());
    if expected != actual {
        let diff = children_perft_diff(expected, actual);
        if !diff.extra_moves.is_empty() || !diff.missing_moves.is_empty() {
            println!("{diff}");
            return Err(path.to_owned());
        } else {
            for wrong_count in diff.wrong_counts.keys() {
                let mut new_path = path.to_vec();
                new_path.push(*wrong_count);
                debug_perft(depth - 1, fen, &new_path)?
            }
        }
    }

    Ok(())
}

#[test]
fn castling_update() {
    let mut game = Game::default();
    let path = ["g2g3", "a7a6", "g1f3", "b7b6", "f1h3", "c7c6"]
        .map(|s| MoveDesc::from_str(s).expect("failed to parse"));
    make_moves(&mut game, &path);
    let updates = game
        .make_move(Move {
            from: Position { rank: 0, file: 4 },
            to: Position { rank: 0, file: 6 },
            move_type: MoveType::KingSideCastle,
        })
        .expect("failed to make move");
    assert_eq!(
        updates,
        vec![
            BoardUpdate::Move {
                from: Position { rank: 0, file: 4 },
                to: Position { rank: 0, file: 6 },
                new_type: PieceType::King,
            },
            BoardUpdate::Move {
                from: Position { rank: 0, file: 7 },
                to: Position { rank: 0, file: 5 },
                new_type: PieceType::Rook,
            },
        ]
    )
}

fn children_perft_diff(
    expected: HashMap<MoveDesc, u64>,
    actual: HashMap<MoveDesc, u64>,
) -> PerftDiff {
    let expected_moves: HashSet<MoveDesc> = expected.keys().cloned().collect();
    let actual_moves: HashSet<MoveDesc> = actual.keys().cloned().collect();

    PerftDiff {
        missing_moves: expected_moves.difference(&actual_moves).cloned().collect(),
        extra_moves: actual_moves.difference(&expected_moves).cloned().collect(),
        wrong_counts: {
            expected_moves
                .intersection(&actual_moves)
                .cloned()
                .map(|m| (m, (expected[&m], actual[&m])))
                .filter(|(_, (e, a))| e != a)
                .collect()
        },
    }
}

struct PerftDiff {
    missing_moves: HashSet<MoveDesc>,
    extra_moves: HashSet<MoveDesc>,
    wrong_counts: HashMap<MoveDesc, (u64, u64)>,
}

impl Display for PerftDiff {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.missing_moves.is_empty() {
            f.write_str("Missing moves:\n")?;
            for m in self.missing_moves.iter().sorted_unstable() {
                format!("\t{m}\n").fmt(f)?;
            }
        }
        if !self.extra_moves.is_empty() {
            f.write_str("Extra moves:\n")?;
            for m in self.extra_moves.iter().sorted_unstable() {
                format!("\t{m}\n").fmt(f)?;
            }
        }

        for (m, (expected_count, actual_count)) in &self.wrong_counts {
            format!("Expected {expected_count} for {m}, got {actual_count}\n").fmt(f)?;
        }
        Ok(())
    }
}

fn make_moves(game: &mut Game, path: &[MoveDesc]) {
    for m in path.iter().cloned() {
        let actual_move = game
            .get_legal_moves(Some(m.from))
            .into_iter()
            .find(|mv| m == *mv)
            .expect(&format!("Move not found: {m}"));
        game.make_move(actual_move).expect("Illegal move");
    }
}

fn stockfish(depth: u8, fen: &Option<&str>, moves: Vec<MoveDesc>) -> HashMap<MoveDesc, u64> {
    let position = match fen {
        Some(fen) => format!("fen {}", fen),
        None => "startpos".to_string(),
    };
    let moves = moves.iter().join(" ");
    let command = format!("position {position} moves {moves}\ngo perft {depth}");
    let mut child = Command::new("stockfish")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to spawn stockfish process");
    let mut stdin = child.stdin.take().expect("failed to get stockfish stdin");
    std::thread::spawn(move || {
        stdin
            .write_all(&command.bytes().collect_vec())
            .expect("failed to write to stockfish stdin");
    });

    let stdout = child
        .wait_with_output()
        .expect("Failed to read stockfish stdout");
    let output = String::from_utf8_lossy(&stdout.stdout);
    output
        .split('\n')
        .filter_map(|line| {
            if line.is_empty() || line.starts_with("Stockfish") || line.starts_with("Nodes") {
                None
            } else {
                let mut parts = line.split(' ');
                Some((
                    parts
                        .next()
                        .expect("move not seen")
                        .strip_suffix(':')
                        .expect("couldn't sptrip ':'")
                        .parse()
                        .expect("failed to parse move"),
                    parts
                        .next()
                        .expect("count not seen")
                        .parse::<u64>()
                        .expect("failed to parse u64"),
                ))
            }
        })
        .collect()
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct MoveDesc {
    from: Position,
    to: Position,
    promote_to: Option<PieceType>,
}

impl From<Move> for MoveDesc {
    fn from(value: Move) -> Self {
        Self {
            from: value.from,
            to: value.to,
            promote_to: value.move_type.promote_to(),
        }
    }
}

impl PartialEq<Move> for MoveDesc {
    fn eq(&self, other: &Move) -> bool {
        self.from == other.from
            && self.to == other.to
            && self.promote_to == other.move_type.promote_to()
    }
}

#[derive(Debug, Error)]
enum MoveParseError {
    #[error("invalid length for move should be 5/4, was {0}")]
    BadLen(usize),
    #[error(transparent)]
    PositionParse(#[from] PositionParseError),
}

impl FromStr for MoveDesc {
    type Err = MoveParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !(4..=5).contains(&(s.len())) {
            return Err(MoveParseError::BadLen(s.len()));
        }
        Ok(MoveDesc {
            from: s[0..2].parse()?,
            to: s[2..4].parse()?,
            promote_to: if s.len() == 5 {
                Some(match &s[4..5] {
                    "q" => PieceType::Queen,
                    "b" => PieceType::Bishop,
                    "n" => PieceType::Knight,
                    "r" => PieceType::Rook,
                    _ => panic!("unexpected promote_to value in move: {}", s),
                })
            } else {
                None
            },
        })
    }
}

impl Display for MoveDesc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.from.fmt(f)?;
        self.to.fmt(f)?;
        if let Some(kind) = self.promote_to {
            f.write_str(match kind {
                PieceType::Queen => "q",
                PieceType::Bishop => "b",
                PieceType::Knight => "n",
                PieceType::Rook => "r",
                _ => panic!("cannot promote to king or pawn"),
            })?;
        }
        Ok(())
    }
}

fn children_perft(board: Game, depth: u8) -> HashMap<MoveDesc, u64> {
    let moves = board.get_legal_moves(None);
    return moves
        .iter()
        .cloned()
        .map(|m| {
            (m.into(), {
                let mut child = board.clone();
                child.make_move(m).expect("move should be legal");
                let mut record = vec![m];
                perft(&child, depth - 1, &mut record)
            })
        })
        .collect();
}

fn perft(board: &Game, depth: u8, record: &mut Vec<Move>) -> u64 {
    let moves = board.get_legal_moves(None);
    let mut total = 0u64;
    if depth == 0 {
        return 1;
    }
    for m in moves {
        record.push(m);
        let mut child = board.clone();
        child.make_move(m).expect("Illegal move");
        total += perft(&child, depth - 1, record);
        record.pop();
    }
    total
}

#[test]
fn perft_all() {
    const DEPTH: usize = 3;
    for i in 0..CASES.len() {
        println!("Testing Perft case: {i}");
        test_perft(i, DEPTH);
    }
}

#[test]
#[ignore = "for debugging purposes, too expensive/fragile to use as test"]
fn debug_with_perft() {
    const CASE: usize = 0;
    const DEPTH: u8 = 1;
    let path = ["g2g3", "a7a6", "g1f3", "b7b6", "f1h3", "c7c6"];
    let fen = CASES[CASE].fen;
    if let Some(fen) = fen {
        println!("Debugging with fen: {fen:?}")
    }
    if let Err(path) = debug_perft(
        DEPTH,
        fen,
        &path.map(|s| MoveDesc::from_str(s).expect("failed to parse")),
    ) {
        panic!(
            "Error on path: {:?}",
            path.into_iter().map(|m| m.to_string()).collect_vec()
        )
    }
}

#[test]
#[ignore]
fn debug() {
    let mut board: Game = CASES[3].fen.try_into().expect("should parse without error");
    assert_eq!(
        board.castling_rights,
        CastlingRights {
            white_king: false,
            white_queen: false,
            black_king: false,
            black_queen: false,
        }
    );
    make_moves(
        &mut board,
        &["e2d1", "b6c4", "e1f1", "c4e3"]
            .map(|s| MoveDesc::from_str(s).expect("move should parse correctly")),
    );
    let moves = board.get_legal_moves(None);
    assert_ne!(moves.into_iter().find(|m| m.to.to_string() == "e3"), None);
}

#[test]
fn test_board_build_array() {
    let game: Game = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - "
        .parse()
        .expect("failed to parse game");
    let arr = game.build_array();
    assert_eq!(arr[0], Some((Colour::White, PieceType::Rook)));
    assert_eq!(arr[1], None);
    assert_eq!(arr[2], None);
    assert_eq!(arr[3], None);
    assert_eq!(arr[4], Some((Colour::White, PieceType::King)));
    assert_eq!(arr[5], None);
    assert_eq!(arr[6], None);
    assert_eq!(arr[7], Some((Colour::White, PieceType::Rook)));
    assert_eq!(arr[8], Some((Colour::White, PieceType::Pawn)));
    assert_eq!(arr[9], Some((Colour::White, PieceType::Pawn)));
    assert_eq!(arr[10], Some((Colour::White, PieceType::Pawn)));
    assert_eq!(arr[11], Some((Colour::White, PieceType::Bishop)));
    assert_eq!(arr[12], Some((Colour::White, PieceType::Bishop)));
    assert_eq!(arr[13], Some((Colour::White, PieceType::Pawn)));
    assert_eq!(arr[14], Some((Colour::White, PieceType::Pawn)));
    assert_eq!(arr[15], Some((Colour::White, PieceType::Pawn)));
    assert_eq!(arr[16], None);
    assert_eq!(arr[17], None);
    assert_eq!(arr[18], Some((Colour::White, PieceType::Knight)));
    assert_eq!(arr[19], None);
    assert_eq!(arr[20], None);
    assert_eq!(arr[21], Some((Colour::White, PieceType::Queen)));
    assert_eq!(arr[22], None);
    assert_eq!(arr[23], Some((Colour::Black, PieceType::Pawn)));
    assert_eq!(arr[24], None);
    assert_eq!(arr[25], Some((Colour::Black, PieceType::Pawn)));
    assert_eq!(arr[26], None);
    assert_eq!(arr[27], None);
    assert_eq!(arr[28], Some((Colour::White, PieceType::Pawn)));
    assert_eq!(arr[29], None);
    assert_eq!(arr[30], None);
    assert_eq!(arr[31], None);
    assert_eq!(arr[32], None);
    assert_eq!(arr[33], None);
    assert_eq!(arr[34], None);
    assert_eq!(arr[35], Some((Colour::White, PieceType::Pawn)));
    assert_eq!(arr[36], Some((Colour::White, PieceType::Knight)));
    assert_eq!(arr[37], None);
    assert_eq!(arr[38], None);
    assert_eq!(arr[39], None);
    assert_eq!(arr[40], Some((Colour::Black, PieceType::Bishop)));
    assert_eq!(arr[41], Some((Colour::Black, PieceType::Knight)));
    assert_eq!(arr[42], None);
    assert_eq!(arr[43], None);
    assert_eq!(arr[44], Some((Colour::Black, PieceType::Pawn)));
    assert_eq!(arr[45], Some((Colour::Black, PieceType::Knight)));
    assert_eq!(arr[46], Some((Colour::Black, PieceType::Pawn)));
    assert_eq!(arr[47], None);
    assert_eq!(arr[48], Some((Colour::Black, PieceType::Pawn)));
    assert_eq!(arr[49], None);
    assert_eq!(arr[50], Some((Colour::Black, PieceType::Pawn)));
    assert_eq!(arr[51], Some((Colour::Black, PieceType::Pawn)));
    assert_eq!(arr[52], Some((Colour::Black, PieceType::Queen)));
    assert_eq!(arr[53], Some((Colour::Black, PieceType::Pawn)));
    assert_eq!(arr[54], Some((Colour::Black, PieceType::Bishop)));
    assert_eq!(arr[55], None);
    assert_eq!(arr[56], Some((Colour::Black, PieceType::Rook)));
    assert_eq!(arr[57], None);
    assert_eq!(arr[58], None);
    assert_eq!(arr[59], None);
    assert_eq!(arr[60], Some((Colour::Black, PieceType::King)));
    assert_eq!(arr[61], None);
    assert_eq!(arr[62], None);
    assert_eq!(arr[63], Some((Colour::Black, PieceType::Rook)));
}

#[derive(Clone, Copy)]
struct PerftCase<'a> {
    fen: Option<&'a str>,
    values: &'a [u64],
}

const CASES: [PerftCase; 7] = [
    PerftCase {
        fen: None,
        values: &[20, 400, 8902, 197281, 4865609, 119060324, 3195901860],
    },
    PerftCase {
        fen: Some("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - "),
        values: &[48, 2039, 97862, 4085603, 193690690, 8031647685],
    },
    PerftCase {
        fen: Some("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - "),
        values: &[48, 2039, 97862, 4085603, 193690690, 8031647685],
    },
    PerftCase {
        fen: Some("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - "),
        values: &[
            14, 191, 2812, 43238, 674624, 11030083, 178633661, 3009794393,
        ],
    },
    PerftCase {
        fen: Some("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"),
        values: &[6, 264, 9467, 422333, 15833292, 706045033],
    },
    PerftCase {
        fen: Some("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"),
        values: &[44, 1486, 62379, 2103487, 89941194],
    },
    PerftCase {
        fen: Some("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10 "),
        values: &[
            46,
            2079,
            89890,
            3894594,
            164075551,
            6923051137,
            287188994746,
            11923589843526,
            490154852788714,
        ],
    },
];
