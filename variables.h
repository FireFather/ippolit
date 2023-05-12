#pragma once
#include <setjmp.h>

static jmp_buf J;

static uint8 jump_ok;
static uint8 move_easy;
static uint8 bad_move;
static uint8 move_battle;
static uint8 analysis_mode;
static uint8 stop_flag;
static uint8 new_game;

static uint32 hash_size = 256;
static uint32 best_move;
static uint32 ponder_move;

static int previous_depth;
static int ok_immediate;
static int stack_height;
static int best_score;
static int best_score_previous;
static int best_depth;
static int depth_limit;

static uint64 clock_start;
static uint64 hash_mask;
static uint64 age;
static uint64 nodes_white;
static uint64 nodes_black;
static uint64 nodes_null;
static uint64 randkey = 1;

static sint64 battle_time;
static sint64 easy_time;
static sint64 normal_time;
static sint64 previous_info;
static sint64 absolute_time;
static sint64 trouble_time;
static sint64 increment_time;

type_position* position;
type_board board;

type_hash* hash_table;
type_pawn_hash* pawn_hash_table;
