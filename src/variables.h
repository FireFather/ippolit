#pragma once
#include <setjmp.h>

static jmp_buf jbuf;

static uint8_t jump_ok;
static uint8_t move_easy;
static uint8_t bad_move;
static uint8_t move_battle;
static uint8_t analysis_mode;
static uint8_t stop_flag;
static uint8_t new_game;

static uint32_t hash_size = 256;
static uint32_t best_move;
static uint32_t ponder_move;

static int previous_depth;
static int ok_immediate;
static int stack_height;
static int best_score;
static int best_score_previous;
static int best_depth;
static int depth_limit;

static uint64_t clock_start;
static uint64_t hash_mask;
static uint64_t age;
static uint64_t nodes_white;
static uint64_t nodes_black;
static uint64_t nodes_null;
static uint64_t randkey = 1;

static int64_t battle_time;
static int64_t easy_time;
static int64_t normal_time;
static int64_t previous_info;
static int64_t absolute_time;
static int64_t trouble_time;
static int64_t increment_time;

static type_position* position;
static type_board board;

static type_hash* hash_table;
static type_pawn_hash* pawn_hash_table;
