#pragma once
typedef enum
{
	castle_white_king = 1,
	castle_white_queen = 2,
	castle_black_king = 4,
	castle_black_queen = 8
} castle_moves;

typedef enum
{
	KQkq = castle_white_king | castle_white_queen | castle_black_king | castle_black_queen,
	Qkq = castle_white_queen | castle_black_king | castle_black_queen,
	Kkq = castle_white_king | castle_black_king | castle_black_queen,
	kq = castle_black_king | castle_black_queen,
	KQk = castle_white_king | castle_white_queen | castle_black_king,
	KQ = castle_white_king | castle_white_queen,
	KQq = castle_white_king | castle_white_queen | castle_black_queen
} KQkq_table;

typedef enum
{
	occupied_white,
	white_pawn,
	white_knight,
	white_king,
	white_king_bishop,
	white_queen_bishop,
	white_rook,
	white_queen,
	occupied_black,
	black_pawn,
	black_knight,
	black_king,
	black_queen_bishop,
	black_king_bishop,
	black_rook,
	black_queen
} pieces;

typedef enum
{
	trans_value,
	gen_captures,
	capture_moves,
	killer_1,
	killer_2,
	ordinary_moves,
	bad_captures,
	trans_value_2,
	gen_captures_2,
	capture_moves_2,
	quiet_checks,
	check_evasions,
	trans_value_3,
	gen_captures_3,
	capture_moves_3,
	quiet_checks_3,
	phase
} phases;

typedef enum
    {
	a1, b1, c1, d1, e1, f1, g1, h1,
	a2, b2, c2, d2, e2, f2, g2, h2,
	a3, b3, c3, d3, e3, f3, g3, h3,
	a4, b4, c4, d4, e4, f4, g4, h4,
	a5, b5, c5, d5, e5, f5, g5, h5,
	a6, b6, c6, d6, e6, f6, g6, h6,
	a7, b7, c7, d7, e7, f7, g7, h7,
	a8, b8, c8, d8, e8, f8, g8, h8,
} squares;

typedef enum
{
	rank_1,
	rank_2,
	rank_3,
	rank_4,
	rank_5,
	rank_6,
	rank_7,
	rank_8
} ranks;

typedef enum
{
	files_a,
	files_b,
	files_c,
	files_d,
	files_e,
	files_f,
	files_g,
	files_h
} files;

typedef enum
{
	value_pawn = 100,
	value_knight = 325,
	value_bishop = 325,
	value_rook = 500,
	value_queen = 975,
	value_king = 12345
} piece_values;
