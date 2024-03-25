#pragma once
#include <cstdint>
#include "struct.h"

char* notate(uint32_t, char*);
char* cp_or_mate(int, char*);
char* write_fen(char*);

int main();
void engine_info();

int eval_material();
int white_king_danger(int);
int black_king_danger(int);

int init_flags(int, int, int, int, int, int, int, int, int, int, int, int, int, int);
int init_weights(int, int, int, int, int, int, int, int, int, int, int, int, int, int);
int calc_white_weights(int, int, int, int, int, int, int, int, int, int, int, int, int, int);
int calc_black_weights(int, int, int, int, int, int, int, int, int, int, int, int, int, int);

int white_root(int, int, int);
int black_root(int, int, int);

int white_all(int, int);
int black_all(int, int);
int white_all_check(int, int);
int black_all_check(int, int);

int white_cut(int, int);
int black_cut(int, int);
int white_cut_check(int, int);
int black_cut_check(int, int);

int white_exclude(int, int, uint32_t);
int white_exclude_check(int, int, uint32_t);
int black_exclude(int, int, uint32_t);
int black_exclude_check(int, int, uint32_t);

int white_low(int, int);
int black_low(int, int);
int white_low_check(int, int);
int black_low_check(int, int);

int white_pv(int, int, int, int);
int black_pv(int, int, int, int);

int white_qsearch(int, int);
int black_qsearch(int, int);
int white_qsearch_check(int, int);
int black_qsearch_check(int, int);

int white_qsearch_pv(int, int, int);
int white_qsearch_pv_check(int, int, int);
int black_qsearch_pv(int, int, int);
int black_qsearch_pv_check(int, int, int);

type_move_list* evasion(type_move_list*, uint64_t);
type_move_list* ordinary(type_move_list*);
type_move_list* capture(type_move_list*, uint64_t);
type_move_list* black_evasion(type_move_list*, uint64_t);
type_move_list* black_capture(type_move_list*, uint64_t);
type_move_list* black_ordinary(type_move_list*);
type_move_list* black_check(type_move_list*, uint64_t);
type_move_list* white_evasion(type_move_list*, uint64_t);
type_move_list* white_capture(type_move_list*, uint64_t);
type_move_list* white_ordinary(type_move_list*);
type_move_list* white_check(type_move_list*, uint64_t);

type_move_list* white_gain(type_move_list*, int);
type_move_list* black_gain(type_move_list*, int);
static type_move_list root_move_list[256];

uint8_t get_input();

uint8_t white_ok(uint32_t move);
uint8_t black_ok(uint32_t move);
uint8_t white_see(uint32_t);
uint8_t black_see(uint32_t);

uint32_t white_next(type_next*);
uint32_t black_next(type_next*);
uint32_t completed_move(uint32_t);

uint64_t get_time();
uint64_t compute_material_value(int, int, int, int, int, int, int, int, int, int);

void eval_mobility();
void eval(int, int, int);
void eval_pawns(type_pawn_hash*);

void hash_exact(int, int, int, int);
void hash_low(uint64_t, int, int, int);
void hash_low_all(int, int, int);
void hash_high(uint64_t, int, int);
void hash_high_cut(int, int);

void make(uint32_t);
void undo(uint32_t);
void white_make(uint32_t);
void white_undo(uint32_t);
void black_make(uint32_t);
void black_undo(uint32_t);

void read_input();
void age_increase();

void init_bitboards();
void init_game();
void init_hash();
void init_pawn_hash_key();
void init_rand_hash();
void init_pawns();
void init_position(char*);
void init_arrays();
void init_captures();
void init_material();

void clear_eval_hash();
void clear_hash();
void clear_history();
void clear_gain();

void parse(char*);
void read_fen(const char*);
void uci();
void readyok();
void read_move(const char* string);

void setup_search(char*);
void search();
void check_if_done(int);
void halt_search();
void send_info(uint64_t);
void information(uint64_t, int);
void output_move();

void white_top();
void black_top();

void do_null();
void undo_null();

void pv_hash(int, int, int);

void make_white_castle(int);
void make_black_castle(int);
void undo_white_castle(int);
void undo_black_castle(int);
void update_white_gain(int);
void update_black_gain(int);

void endgame_pawn_white(int, uint8_t, type_pawn_hash*);
void endgame_pawn_black(int, uint8_t, type_pawn_hash*);

void calculate_material_value(int);
void sort(const type_move_list*, type_move_list*, uint32_t, uint32_t, uint32_t);
