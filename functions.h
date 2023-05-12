#pragma once
char* notate(uint32, char*);
char* cp_or_mate(int, char*);
char* write_fen(char*);

int main(void);

int eval_material(void);
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

int white_exclude(int, int, uint32);
int white_exclude_check(int, int, uint32);
int black_exclude(int, int, uint32);
int black_exclude_check(int, int, uint32);

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

type_move_list* evasion(type_move_list*, uint64);
type_move_list* ordinary(type_move_list*);
type_move_list* capture(type_move_list*, uint64);
type_move_list* black_evasion(type_move_list*, uint64);
type_move_list* black_capture(type_move_list*, uint64);
type_move_list* black_ordinary(type_move_list*);
type_move_list* black_check(type_move_list*, uint64);
type_move_list* white_evasion(type_move_list*, uint64);
type_move_list* white_capture(type_move_list*, uint64);
type_move_list* white_ordinary(type_move_list*);
type_move_list* white_check(type_move_list*, uint64);

type_move_list* white_gain(type_move_list*, int);
type_move_list* black_gain(type_move_list*, int);
type_move_list root_move_list[256];

uint8 get_input(void);

uint8 white_ok(uint32 move);
uint8 black_ok(uint32 move);
uint8 white_see(uint32);
uint8 black_see(uint32);

uint32 white_next(type_next*);
uint32 black_next(type_next*);
uint32 completed_move(uint32);

uint64 get_time(void);
uint64 compute_material_value(int, int, int, int, int, int, int, int, int, int);

void eval_mobility(void);
void eval(int, int, int);
void eval_pawns(type_pawn_hash*);

void hash_exact(int, int, int, int);
void hash_low(uint64, int, int, int);
void hash_low_all(int, int, int);
void hash_high(uint64, int, int);
void hash_high_cut(int, int);

void make(uint32);
void undo(uint32);
void white_make(uint32);
void white_undo(uint32);
void black_make(uint32);
void black_undo(uint32);

void input(void);
void age_increase(void);

void init_bitboards(void);
void init_game(void);
void init_hash(void);
void init_pawn_hash_key(void);
void init_rand_hash(void);
void init_pawns(void);
void init_position(char*);
void init_arrays(void);
void init_captures(void);
void init_material(void);

void clear_eval_hash(void);
void clear_hash(void);
void clear_history(void);
void clear_gain(void);

void parse(char*);
void read_fen(const char*);
void uci(void);
void readyok(void);
void read_move(const char* string);

void setup_search(char*);
void search(void);
void check_if_done(int);
void halt_search(void);
void send_info(uint64);
void information(uint64, int);
void output_move(void);

void white_top(void);
void black_top(void);

void do_null(void);
void undo_null(void);

void pv_hash(int, int, int);

void make_white_castle(int);
void make_black_castle(int);
void undo_white_castle(int);
void undo_black_castle(int);
void update_white_gain(int);
void update_black_gain(int);

void endgame_pawn_white(int, uint8, type_pawn_hash*);
void endgame_pawn_black(int, uint8, type_pawn_hash*);

void calculate_material_value(int);
void sort(const type_move_list*, type_move_list*, uint32, uint32, uint32);
