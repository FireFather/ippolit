
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
	A1, B1, C1, D1, E1, F1, G1, H1,
	A2, B2, C2, D2, E2, F2, G2, H2,
	A3, B3, C3, D3, E3, F3, G3, H3,
	A4, B4, C4, D4, E4, F4, G4, H4,
	A5, B5, C5, D5, E5, F5, G5, H5,
	A6, B6, C6, D6, E6, F6, G6, H6,
	A7, B7, C7, D7, E7, F7, G7, H7,
	A8, B8, C8, D8, E8, F8, G8, H8,
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
    files_A,
    files_B,
    files_C,
    files_D,
    files_E,
    files_F,
    files_G,
    files_H
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