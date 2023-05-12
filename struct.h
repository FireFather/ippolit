#pragma once
typedef struct
{
	uint64 edge, middle, center;
	uint8 shelter_edge[8], shelter_middle[8], shelter_center[8];
	uint8 storm_edge[8], storm_middle[8], storm_center[8], shelter_diag[8];
	uint8 score_is_zero, set_score_zero;
} type_shelter_storm;

typedef struct
{
	uint64 pawn_hash_key;
	uint8 white_pawn_white, white_pawn_black, black_pawn_white, black_pawn_black;
	uint8 white_passed_pawn_file, black_passed_pawn_file, white_draw_weight, black_draw_weight;
	uint32 white_king_danger, black_king_danger, score, _0;
} type_pawn_hash;

typedef struct
{
	uint32 hash_key;
	uint8 flag, age;
	uint8 depth_high, depth_low;
	uint16 move;
	sint16 score_low;
	uint16 _0;
	sint16 score_high;
} type_hash;

typedef struct
{
	uint64 hash_key;
	sint32 score;
	uint16 move;
	uint8 depth, age;
} type_PV_hash;

typedef struct
{
	uint32 move;
} type_move_list;

typedef struct
{
	uint8 square[64];
	uint64 piece[16];
	uint64 occupied_total, occupied_90_left, occupied_45_left, occupied_45_right;
	uint8 white_to_move, white_king, black_king, castle;
} type_board;

typedef struct
{
	uint64 hash_key, pawn_hash_key;
	uint32 material, pst_value, _7;
	uint8 castle, reversible, en_passant, capture;
	uint64 white_attack, black_attack, white_xray, black_xray, _9, _8;
	sint32 score, positional_value;
	uint16 _5, _6, killer_1, killer_2, move;
	uint8 _0, _3, _4, lazy, saved_flags, flag;
	uint64 white_king_check, black_king_check, _1;
} type_position;

typedef struct
{
	int phase, mask, bad_capture;
	uint32 trans_move, move;
	uint64 target;
	type_move_list list[256];
	uint32 bad_captures[64];
} type_next;

typedef struct
{
	sint16 value;
	uint8 token, flag;
} type_material;
