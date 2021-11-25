static char string_1[64];
static char string_2[64];
static char start_position[80] = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

static int line_turn[4][64];
static int length_[64];
static int where_[64];
static int left_54[64];
static int right_54[64];
static int left_09[64];

static uint8 xray_table[64];

static sint8 direction[64][64];
static sint16 max_increase[16][07777];
static sint32 capture_value[16][16];

static uint16 history_table[16][64];
static uint64 stack[1024];
static uint64 eval_hash[(0x8000)];

static uint64 square_set[64];
static uint64 square_clear[64];

static uint64 non_diagonal[64];
static uint64 non_orthogonal[64];
static uint64 orthogonal[64];
static uint64 diagonal[64];
static uint64 diagonal_and_orthogonal[64];

static uint64 open_file_white[64];
static uint64 open_file_black[64];

static uint64 passed_pawn_white[64];
static uint64 passed_pawn_black[64];
static uint64 pawn_protected_white[64];
static uint64 pawn_protected_black[64];
static uint64 pawn_isolated_black[64];
static uint64 pawn_isolated_white[64];
static uint64 passed_pawn_connected[64];

static uint64 in_front_white[8];
static uint64 not_in_front_white[8];
static uint64 in_front_black[8];
static uint64 not_in_front_black[8];

static uint64 files_left[8];
static uint64 files_right[8];

static uint64 west_one[64];
static uint64 east_one[64];
static uint64 west_two[64];
static uint64 east_two[64];
static uint64 nudging[64];

static uint64 diagonal_length[64];
static uint64 files_isolated[8];
static uint64 table_gain[64];

static uint64 quadrant_white_wtm[64];
static uint64 quadrant_black_wtm[64];
static uint64 quadrant_white_btm[64];
static uint64 quadrant_black_btm[64];
static uint64 shepherd_white[64];
static uint64 shepherd_black[64];

static uint64 interposition_table[64][64];
static uint64 evasion_table[64][64];

static uint64 left_90_set[64];
static uint64 left_45_set[64];
static uint64 right_45_set[64];
static uint64 left_90_clear[64];
static uint64 left_45_clear[64];
static uint64 right_45_clear[64];

static uint64 rand_hash_castle[16];
static uint64 rand_hash_table[16][64];
static uint64 rand_hash_en_passant[8];
static uint64 rand_hash_white_to_move;

static uint64 attack_knight[64];
static uint64 attack_king[64];
static uint64 attack_pawn_white[64];
static uint64 attack_pawn_black[64];
static uint64 bitboard_line_obscured[4][64][64];

type_position root_position[1024];
type_PV_hash PV_hash_table[65536];

type_material material_table[419904];
type_shelter_storm shelter_storm[8];