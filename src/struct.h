#pragma once
using type_shelter_storm = struct {
  uint64_t edge, middle, center;
  uint8_t shelter_edge[8], shelter_middle[8], shelter_center[8];
  uint8_t storm_edge[8], storm_middle[8], storm_center[8], shelter_diag[8];
  uint8_t score_is_zero, set_score_zero;
};

using type_pawn_hash = struct {
  uint64_t pawn_hash_key;
  uint8_t white_pawn_white, white_pawn_black, black_pawn_white, black_pawn_black;
  uint8_t white_passed_pawn_file, black_passed_pawn_file, white_draw_weight, black_draw_weight;
  uint32_t white_king_danger, black_king_danger, score, _0;
};

using type_hash = struct {
  uint32_t hash_key;
  uint8_t flag, age;
  uint8_t depth_high, depth_low;
  uint16_t move;
  int16_t score_low;
  uint16_t _0;
  int16_t score_high;
};

using type_pv_hash = struct {
  uint64_t hash_key;
  int32_t score;
  uint16_t move;
  uint8_t depth, age;
};

using type_move_list = struct {
  uint32_t move;
};

using type_board = struct {
  uint8_t square[64];
  uint64_t piece[16];
  uint64_t occupied_total, occupied_90_left, occupied_45_left, occupied_45_right;
  uint8_t white_to_move, white_king, black_king, castle;
};

using type_position = struct {
  uint64_t hash_key, pawn_hash_key;
  uint32_t material, pst_value, _7;
  uint8_t castle, reversible, en_passant, capture;
  uint64_t white_attack, black_attack, white_xray, black_xray, _9, _8;
  int32_t score, positional_value;
  uint16_t _5, _6, killer_1, killer_2, move;
  uint8_t _0, _3, _4, lazy, saved_flags, flag;
  uint64_t white_king_check, black_king_check, _1;
};

using type_next = struct {
  int phase, mask, bad_capture;
  uint32_t trans_move, move;
  uint64_t target;
  type_move_list list[256];
  uint32_t bad_captures[64];
};

using type_material = struct {
  int16_t value;
  uint8_t token, flag;
};
