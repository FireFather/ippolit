
static char* notate( uint32, char* );
static char* cp_or_mate( int, char* );
static char* write_fen( char* );

int main( void );

static int eval_material( void );
static int white_king_danger( int );
static int black_king_danger( int );

static int init_flags( int, int, int, int, int, int, int, int, int, int, int, int, int, int );
static int init_weights( int, int, int, int, int, int, int, int, int, int, int, int, int, int );
static int calc_white_weights( int, int, int, int, int, int, int, int, int, int, int, int, int, int );
static int calc_black_weights( int, int, int, int, int, int, int, int, int, int, int, int, int, int );

static int white_root( int, int, int );
static int black_root( int, int, int );

static int white_all( int, int );
static int black_all( int, int );
static int white_all_check( int, int );
static int black_all_check( int, int );

static int white_cut( int, int );
static int black_cut( int, int );
static int white_cut_check( int, int );
static int black_cut_check( int, int );

static int white_exclude( int, int, uint32 );
static int white_exclude_check( int, int, uint32 );
static int black_exclude( int, int, uint32 );
static int black_exclude_check( int, int, uint32 );

static int white_low( int, int );
static int black_low( int, int );
static int white_low_check( int, int );
static int black_low_check( int, int );

static int white_pv( int, int, int, int );
static int black_pv( int, int, int, int );

static int white_qsearch( int, int );
static int black_qsearch( int, int );
static int white_qsearch_check( int, int );
static int black_qsearch_check( int, int );

static int white_qsearch_pv( int, int, int );
static int white_qsearch_pv_check( int, int, int );
static int black_qsearch_pv( int, int, int );
static int black_qsearch_pv_check( int, int, int );

static type_move_list* evasion( type_move_list*, uint64 );
static type_move_list* ordinary( type_move_list* );
static type_move_list* capture( type_move_list*, uint64 );
static type_move_list* black_evasion( type_move_list*, uint64 );
static type_move_list* black_capture( type_move_list*, uint64 );
static type_move_list* black_ordinary( type_move_list* );
static type_move_list* black_check( type_move_list*, uint64 );
static type_move_list* white_evasion( type_move_list*, uint64 );
static type_move_list* white_capture( type_move_list*, uint64 );
static type_move_list* white_ordinary( type_move_list* );
static type_move_list* white_check( type_move_list*, uint64 );
static type_move_list* white_capture( type_move_list*, uint64 );
static type_move_list* black_capture( type_move_list*, uint64 );
static type_move_list* white_ordinary( type_move_list* );
static type_move_list* black_ordinary( type_move_list* );
static type_move_list* capture( type_move_list*, uint64 );
static type_move_list* ordinary( type_move_list* );
static type_move_list* evasion( type_move_list*, uint64 );
static type_move_list* black_evasion( type_move_list*, uint64 );
static type_move_list* white_evasion( type_move_list*, uint64 );
static type_move_list* white_check( type_move_list*, uint64 );
static type_move_list* black_check( type_move_list*, uint64 );
static type_move_list* white_gain( type_move_list*, int );
static type_move_list* black_gain( type_move_list*, int );
static type_move_list root_move_list[256];

static uint8 get_input( void );

static uint8 white_ok( uint32 move );
static uint8 black_ok( uint32 move );
static uint8 white_see( uint32 );
static uint8 black_see( uint32 );

static uint16 rand16( void );
static uint64 rand64( void );

static uint32 white_next( type_next* );
static uint32 black_next( type_next* );
static uint32 completed_move( uint32 );

static uint16 rand16( void );
static uint64 rand64( void );
static uint64 get_time( void );
static uint64 compute_material_value( int, int, int, int, int, int, int, int, int, int );

static void eval_mobility( void );
static void eval( int, int, int );
static void eval_pawns( type_pawn_hash* );

static void hash_exact( int, int, int, int );
static void hash_low( uint64, int, int, int );
static void hash_low_all( int, int, int );
static void hash_high( uint64, int, int );
static void hash_high_cut( int, int );

static void make( uint32 );
static void undo( uint32 );
static void white_make( uint32 );
static void white_undo( uint32 );
static void black_make( uint32 );
static void black_undo( uint32 );

static void input( void );
static void age_increase( void );

static void init_bitboards( void );
static void init_game( void );
static void init_hash( void );
static void init_pawn_hash_key( void );
static void init_rand_hash( void );
static void init_pawns( void );
static void init_position( char* );
static void init_arrays( void );
static void init_captures( void );
static void init_material( void );

static void clear_eval_hash( void );
static void clear_hash( void );
static void clear_history( void );
static void clear_gain( void );

static void parse( char* );
static void read_fen( char* );
static void uci( void );
static void readyok( void );
static void read_move( char* I );

static void setup_search( char* );
static void search( void );
static void check_if_done( int );
static void halt_search( void );
static void send_info( uint64 );
static void information( uint64, int);
static void output_move( void );

static void white_top( void );
static void black_top( void );

static void do_null( void );
static void undo_null( void );

static void pv_hash( int, int, int );

static void make_white_castle( int );
static void make_black_castle( int );
static void undo_white_castle( int );
static void undo_black_castle( int );
static void update_white_gain( int );
static void update_black_gain( int );

static void endgame_pawn_white( int, uint8, type_pawn_hash* );
static void endgame_pawn_black( int, uint8, type_pawn_hash* );

static void calculate_material_value( int );
static void sort( type_move_list*, type_move_list*, uint32, uint32, uint32 );