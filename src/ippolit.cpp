#include <cstdio>
#include <csetjmp>
#include <cstring>
#include <cstdint>

#if !defined(__arm__) && !defined(__linux__)
#include <windows.h>

uint64_t get_time()
{
  constexpr uint64_t y = 1000;
	const uint64_t x = GetTickCount64() * y;
	return x;
}

#else
#include <unistd.h>
#include <sys/time.h>
#include <sys/select.h>
#include <stdlib.h>
#include <sys/time.h>

uint64_t get_time()
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  constexpr uint64_t y = 1000;
  const uint64_t x = (tv.tv_sec * 1000000 + tv.tv_usec) / 1000 * y;
  return x;
}

#include <stdlib.h>

int64_t _atoi64(const char *str) {
    return atoll(str);
}

#endif

#include "pragma.h"
#include "bitscan.h"
#include "macros.h"
#include "enum.h"
#include "struct.h"
#include "arrays.h"
#include "constants.h"
#include "defines.h"
#include "variables.h"
#include "rand.h"
#include "functions.h"

#if !defined(__arm__) && !defined(__linux__)
uint8_t get_input()
{

	static int init = 0, is_pipe;
	static HANDLE stdin_h;
	DWORD value;

	if (!init)
	{
		init = 1;
		stdin_h = GetStdHandle(STD_INPUT_HANDLE);
		is_pipe = !GetConsoleMode(stdin_h, &value);

		if (!is_pipe)
		{
			SetConsoleMode(stdin_h, value & ~(ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT));
			FlushConsoleInputBuffer(stdin_h);
		}
	}

	if (is_pipe)
	{
		if (!PeekNamedPipe(stdin_h, nullptr, 0, nullptr, &value, nullptr))
			return 1;
		return value > 0;
	}
	GetNumberOfConsoleInputEvents(stdin_h, &value);
	return value > 1;
	
}
	
	#else
	struct timeval tv;
	uint8_t get_input()
{
		
	fd_set readfds;

	FD_ZERO (&readfds);
	FD_SET (fileno(stdin), &readfds);
	tv.tv_sec=0; tv.tv_usec=0;
    select(16, &readfds, 0, 0, &tv);

	return (FD_ISSET(fileno(stdin), &readfds));
	
}

#endif
void do_null() {
  nodes_null++;
  position->saved_flags = position->flag;
  memcpy(position + 1, position, 64);
  position++;
  position->hash_key ^= rand_hash_white_to_move;
  board.white_to_move ^= 1;
  position->reversible++;

  if (position->en_passant) {
    position->hash_key ^= rand_hash_en_passant[position->en_passant & 7];
    position->en_passant = 0;
  }
  position->score = -((position - 1)->score + 5);
  position->positional_value = (position - 1)->positional_value;
  position->lazy = (position - 1)->lazy;
  position->flag &= ~3;
  position->move = 0;
  stack[++stack_height] = position->hash_key;
}

void undo_null() {
  position--;
  stack_height--;
  board.white_to_move ^= 1;
  position->flag = position->saved_flags;
}

char* notate(const uint32_t move, char* string) {
  constexpr char c[9] = "0123nbrq";
  const int from = move >> 6 & 077;
  const int to = move & 077;

  if (move == 0) {
    string[0] = 'N';
    string[1] = 'U';
    string[2] = string[3] = 'L';
    string[4] = 0;
    return string;
  }
  sprintf(string, "%c%c%c%c", 'a' + (from & 7), '1' + (from >> 3 & 7), 'a' + (to & 7), '1' + (to >> 3 & 7));

  if ((move & 070000) >= 040000) {
    const int pr = (move & 070000) >> 12;
    sprintf(string + 4, "%c", c[pr]);
  }
  return string;
}

void init_bitboards() {
  int i, piece;

  for (i = 0; i < 16; i++)
    board.piece[i] = 0;
  position->hash_key = position->pawn_hash_key = 0;
  position->material = 0;
  position->pst_value = 0;

  for (i = a1; i <= h8; i++) {
    if ((piece = board.square[i])) {
      position->pst_value += pst[piece][i];
      position->hash_key ^= rand_hash_table[piece][i];

      if (piece == white_pawn || piece == black_pawn)
        position->pawn_hash_key ^= rand_hash_table[piece][i];
      position->material += material_values[piece];
      board.piece[board.square[i]] |= static_cast<uint64_t>(1) << i;
    }
  }
  board.piece[occupied_white] = board.piece[white_king] | board.piece[white_queen]
    | board.piece[white_rook] | (board.piece[white_king_bishop] | board.piece[white_queen_bishop])
    | board.piece[white_knight] | board.piece[white_pawn];
  board.piece[occupied_black] = board.piece[black_king] | board.piece[black_queen]
    | board.piece[black_rook] | (board.piece[black_queen_bishop] | board.piece[black_king_bishop])
    | board.piece[black_knight] | board.piece[black_pawn];
  board.occupied_total = board.piece[occupied_white] | board.piece[occupied_black];
  board.occupied_90_left = board.occupied_45_left = board.occupied_45_right = 0;
  uint64_t O = board.occupied_total;

  if (popcnt(board.piece[white_queen]) > 1
    || popcnt(board.piece[black_queen]) > 1
    || popcnt(board.piece[white_rook]) > 2
    || popcnt(board.piece[black_rook]) > 2
    || popcnt(board.piece[white_king_bishop]) > 1
    || popcnt(board.piece[black_queen_bishop]) > 1
    || popcnt(board.piece[white_knight]) > 2
    || popcnt(board.piece[black_knight]) > 2
    || popcnt(board.piece[white_queen_bishop]) > 1
    || popcnt(board.piece[black_king_bishop]) > 1)
    position->material |= 0x80000000;

  while (O) {
    const int b = bsf(O);
    O &= O - 1;
    board.occupied_90_left |= static_cast<uint64_t>(1) << left_90[b];
    board.occupied_45_left |= static_cast<uint64_t>(1) << left_45[b];
    board.occupied_45_right |= static_cast<uint64_t>(1) << right_45[b];
  }
  board.white_king = static_cast<uint8_t>(bsf(board.piece[white_king]));
  board.black_king = static_cast<uint8_t>(bsf(board.piece[black_king]));

  position->hash_key ^= rand_hash_castle[position->castle];

  if (position->en_passant)
    position->hash_key ^= rand_hash_en_passant[position->en_passant & 7];
  position->pawn_hash_key ^= rand_hash_castle[position->castle] ^ 0x74d3c012a8bf965e
    ^ rand_hash_table[white_king][board.white_king]
    ^ rand_hash_table[black_king][board.black_king];

  if (board.white_to_move)
    position->hash_key ^= rand_hash_white_to_move;
  eval(-0x7fff0000, 0x7fff0000, 0);
}

void init_game() {
  int i;

  for (i = a1; i <= h8; i++)
    board.square[i] = 0;
  memset(root_position, 0, 256 * sizeof(type_position));
  position = root_position;
  board.white_to_move = 1;
  position->castle = 0x0f;
  position->en_passant = 0;
  position->reversible = 0;

  for (i = a2; i <= h2; i++)
    board.square[i] = white_pawn;

  for (i = a7; i <= h7; i++)
    board.square[i] = black_pawn;
  board.square[d1] = white_queen;
  board.square[d8] = black_queen;
  board.square[e1] = white_king;
  board.square[e8] = black_king;
  board.square[a1] = board.square[h1] = white_rook;
  board.square[a8] = board.square[h8] = black_rook;
  board.square[b1] = board.square[g1] = white_knight;
  board.square[b8] = board.square[g8] = black_knight;
  board.square[c1] = white_queen_bishop;
  board.square[f1] = white_king_bishop;
  board.square[c8] = black_queen_bishop;
  board.square[f8] = black_king_bishop;
  previous_depth = 1000;
  ok_immediate = 0;
  new_game = 1;
  init_bitboards();
  clear_hash();
  clear_eval_hash();
  clear_history();
  clear_gain();
}

void read_fen(const char* string) {
  int rank = 7, file = 0, c = 0;

  for (int i = a1; i <= h8; i++)
    board.square[i] = 0;

  while (true) {
    const int piece = string[c++];

    if (piece == 0)
      return;

    switch (piece) {
      case '/': rank--;
        file = 0;
        break;

      case 'p': board.square[file + 8 * rank] = black_pawn;
        file++;
        break;

      case 'b': if (square_set[file + 8 * rank] & 0xaa55aa55aa55aa55)
          board.square[file + 8 * rank] = black_king_bishop;
        else
          board.square[file + 8 * rank] = black_queen_bishop;
        file++;
        break;

      case 'n': board.square[file + 8 * rank] = black_knight;
        file++;
        break;

      case 'r': board.square[file + 8 * rank] = black_rook;
        file++;
        break;

      case 'q': board.square[file + 8 * rank] = black_queen;
        file++;
        break;

      case 'k': board.square[file + 8 * rank] = black_king;
        file++;
        break;

      case 'P': board.square[file + 8 * rank] = white_pawn;
        file++;
        break;

      case 'B': if (square_set[file + 8 * rank] & 0xaa55aa55aa55aa55)
          board.square[file + 8 * rank] = white_queen_bishop;
        else
          board.square[file + 8 * rank] = white_king_bishop;
        file++;
        break;

      case 'N': board.square[file + 8 * rank] = white_knight;
        file++;
        break;

      case 'R': board.square[file + 8 * rank] = white_rook;
        file++;
        break;

      case 'Q': board.square[file + 8 * rank] = white_queen;
        file++;
        break;

      case 'K': board.square[file + 8 * rank] = white_king;
        file++;
        break;

      case '1': file += 1;
        break;

      case '2': file += 2;
        break;

      case '3': file += 3;
        break;

      case '4': file += 4;
        break;

      case '5': file += 5;
        break;

      case '6': file += 6;
        break;

      case '7': file += 7;
        break;

      case '8': file += 8;
        break;

      default: break;
    }

    if (rank == 0 && file >= 8)
      break;
  }
}

char* write_fen(char* string) {
  char i[1024];
  uint8_t ok = 0;
  int en_passant;
  sscanf(string, "%s", i);
  read_fen(i);
  memset(root_position, 0, 0x100 * sizeof(type_position));
  position = root_position;
  string += strlen(i) + 1;
  sscanf(string, "%s", i);

  if (i[0] == 'w')
    board.white_to_move = 1;

  else if (i[0] == 'b')
    board.white_to_move = 0;

  string += strlen(i) + 1;
  sscanf(string, "%s", i);
  position->castle = 16;

  if (!strcmp(i, "KQkq"))
    position->castle = 15;

  if (!strcmp(i, "Qkq"))
    position->castle = 14;

  if (!strcmp(i, "Kkq"))
    position->castle = 13;

  if (!strcmp(i, "kq"))
    position->castle = 12;

  if (!strcmp(i, "KQq"))
    position->castle = 11;

  if (!strcmp(i, "Qq"))
    position->castle = 10;

  if (!strcmp(i, "Kq"))
    position->castle = 9;

  if (!strcmp(i, "q"))
    position->castle = 8;

  if (!strcmp(i, "KQk"))
    position->castle = 7;

  if (!strcmp(i, "Qk"))
    position->castle = 6;

  if (!strcmp(i, "Kk"))
    position->castle = 5;

  if (!strcmp(i, "k"))
    position->castle = 4;

  if (!strcmp(i, "KQ"))
    position->castle = 3;

  if (!strcmp(i, "Q"))
    position->castle = 2;

  if (!strcmp(i, "K"))
    position->castle = 1;

  if (!strcmp(i, "-"))
    position->castle = 0;

  string += strlen(i) + 1;
  sscanf(string, "%s", i);
  position->en_passant = 0;

  if (!strcmp(i, "-"))
    en_passant = 0;
  else {
    en_passant = i[0] - 'a' + 8 * (i[1] - '1');
    ok = 0;
  }

  if (en_passant) {
    if (board.white_to_move) {
      if ((en_passant & 7) != files_a && board.square[en_passant - 9] == white_pawn)
        ok = 1;

      if ((en_passant & 7) != files_h && board.square[en_passant - 7] == white_pawn)
        ok = 1;
    }
    else {
      if ((en_passant & 7) != files_a && board.square[en_passant + 7] == black_pawn)
        ok = 1;

      if ((en_passant & 7) != files_h && board.square[en_passant + 9] == black_pawn)
        ok = 1;
    }

    if (ok)
      position->en_passant = en_passant;
  }
  string += strlen(i) + 1;
  sscanf(string, "%s", i);
  position->reversible = static_cast<uint8_t>(atoi(i));
  string += strlen(i) + 1;
  sscanf(string, "%s", i);
  string += strlen(i) + 1;
  init_bitboards();
  return string;
}

uint32_t completed_move(uint32_t x) {
  const int to = x & 077, from = x >> 6 & 077;

  if (!x)
    return x;

  const int piece = board.square[from];

  if (piece == white_king || piece == black_king) {
    if (to - from == 2 || from - to == 2)
      x |= 010000;
  }

  if ((x & 077) != 0 && (x & 077) == position->en_passant
    && (piece == white_pawn || piece == black_pawn))
    x |= 030000;
  return x;
}

void read_move(const char* string) {
  type_move_list* move_list;

  while (string[0]) {
    char T[256];
    type_move_list list[256];
    eval_mobility();

    if (board.white_to_move
          ? position->black_attack & board.piece[white_king]
          : position->white_attack & board.piece[black_king]) {
      move_list = evasion(list, 0xffffffffffffffff);
      move_list++;
    }
    else {
      move_list = capture(list, board.occupied_total);
      move_list = ordinary(move_list);
    }
    uint32_t completion = completed_move(
      (string[2] - 'a') + ((string[3] - '1') << 3) + ((string[0] - 'a') << 6) + ((string[1] - '1') << 9));
    sscanf(string, "%s", T);

    if (strlen(T) == 5) {
      if (string[4] == 'b')
        completion |= 050000;

      if (string[4] == 'n')
        completion |= 040000;

      if (string[4] == 'r')
        completion |= 060000;

      if (string[4] == 'q')
        completion |= 070000;
    }

    for (int i = 0; i < move_list - list; i++) {
      if (completion == (list[i].move & 0x7fff)) {
        make(completion);
        break;
      }
    }

    string += strlen(T) + 1;

    while (string[0] == ' ')
      string++;
  }
}

void init_position(char* string) {
  char i[1024];
  nodes_white = nodes_black = nodes_null = 0;
  sscanf(string, "%s", i);

  if (!strcmp(i, "startpos")) {
    write_fen(start_position);
    string += strlen("startpos") + 1;
  }

  if (!strcmp(i, "fen")) {
    string += strlen("fen") + 1;
    string = write_fen(string);
  }

  if (string[0]) {
    sscanf(string, "%s", i);

    if (!strcmp(i, "moves")) {
      string += strlen("moves") + 1;
      read_move(string);
    }
  }

  if (new_game)
    clear_gain();
}

void init_rand_hash() {
  int i, j;
  rand_hash_white_to_move = rand64();
  rand_hash_castle[0] = 0;
  rand_hash_castle[1] = rand64();
  rand_hash_castle[2] = rand64();
  rand_hash_castle[4] = rand64();
  rand_hash_castle[8] = rand64();

  for (i = 0; i < 16; i++) {
    if (popcnt(i) < 2)
      continue;
    rand_hash_castle[i] = 0;

    for (j = 1; j < 16; j <<= 1)
      if (i & j)
        rand_hash_castle[i] ^= rand_hash_castle[j];
  }

  for (i = 0; i < 16; i++)
    for (j = a1; j <= h8; j++)
      rand_hash_table[i][j] = rand64();

  for (i = files_a; i <= files_h; i++)
    rand_hash_en_passant[i] = rand64();
}

void init_arrays() {
  int l, w, i, square = 0, square_2, j, u, file, rank, king_square, dir;
  uint64_t T, b, s;

  for (i = a1; i <= h8; i++) {
    line_turn[0][i] = shift[left_45[i]];
    line_turn[1][i] = shift[right_45[i]];
  }

  for (i = a1; i <= h8; i++) {
    line_turn[2][i] = 1 + (i & 56);
    line_turn[3][i] = 1 + (left_90[i] & 56);
  }

  for (i = 1; i <= 8; i++)
    for (j = 1; j <= i; j++) {
      length[square] = i;
      where[square++] = j - 1;
    }

  for (i = 7; i >= 1; i--)
    for (j = 1; j <= i; j++) {
      length[square] = i;
      where[square++] = j - 1;
    }

  for (i = a1; i <= h8; i++) {
    left_54[left_45[i]] = i;
    left_09[left_90[i]] = i;
    right_54[right_45[i]] = i;
  }

  for (i = a1; i <= h8; i++) {
    square_set[i] = 0;
    square_set[i] |= static_cast<uint64_t>(1) << i;
    square_clear[i] = ~square_set[i];
  }

  for (i = a1; i <= h8; i++) {
    left_90_set[i] = 0;
    left_90_set[i] |= static_cast<uint64_t>(1) << left_90[i];
    left_90_clear[i] = ~left_90_set[i];
    left_45_set[i] = 0;
    left_45_set[i] |= static_cast<uint64_t>(1) << left_45[i];
    left_45_clear[i] = ~left_45_set[i];
    right_45_set[i] = 0;
    right_45_set[i] |= static_cast<uint64_t>(1) << right_45[i];
    right_45_clear[i] = ~right_45_set[i];
  }

  for (i = a1; i <= h8; i++) {
    attack_knight[i] = 0;

    for (j = 0; j < 8; j++) {
      square = i + jumps[j];

      if (square < a1 || square > h8)
        continue;

      if (FILE_DISTANCE(i, square) > 2 || RANK_DISTANCE(i, square) > 2)
        continue;

      attack_knight[i] |= static_cast<uint64_t>(1) << square;
    }
  }

  for (i = a1; i <= h8; i++) {
    attack_king[i] = 0;

    for (j = a1; j <= h8; j++) {
      if (MAX(FILE_DISTANCE(i, j), RANK_DISTANCE(i, j)) == 1)
        attack_king[i] |= static_cast<uint64_t>(1) << j;
    }
  }

  for (i = a1; i <= h1; i++) {
    attack_pawn_white[i] = 0;
    attack_pawn_black[i] = square_set[i + 7] | square_set[i + 9];
  }

  for (i = a2; i <= h7; i++) {
    attack_pawn_white[i] = square_set[i - 7] | square_set[i - 9];
    attack_pawn_black[i] = square_set[i + 7] | square_set[i + 9];
  }

  for (i = a8; i <= h8; i++) {
    attack_pawn_black[i] = 0;
    attack_pawn_white[i] = square_set[i - 7] | square_set[i - 9];
  }

  for (i = a1; i <= a8; i += 8) {
    attack_pawn_white[i] = square_set[i - 7];
    attack_pawn_black[i] = square_set[i + 9];
  }

  for (i = h1; i <= h8; i += 8) {
    attack_pawn_white[i] = square_set[i - 9];
    attack_pawn_black[i] = square_set[i + 7];
  }
  attack_pawn_white[a1] = 0;
  attack_pawn_white[a2] = square_set[b1];
  attack_pawn_black[a7] = square_set[b8];
  attack_pawn_black[a8] = 0;
  attack_pawn_white[h1] = 0;
  attack_pawn_white[h2] = square_set[g1];
  attack_pawn_black[h7] = square_set[g8];
  attack_pawn_black[h8] = 0;

  files_isolated[files_a] = 0x0202020202020202;
  files_isolated[files_h] = 0x4040404040404040;

  for (file = files_b; file <= files_g; file++)
    files_isolated[file] = file_table[file - 1] | file_table[file + 1];

  for (square = a1; square <= h8; square++) {
    pawn_isolated_white[square] = 0;
    pawn_isolated_black[square] = 0;
    file = square & 7;
    rank = square >> 3;

    if (rank < rank_8)
      pawn_isolated_white[square] |= files_isolated[file] & rank_table[rank + 1];

    if (rank < rank_7)
      pawn_isolated_white[square] |= files_isolated[file] & rank_table[rank + 2];

    if (rank > rank_1)
      pawn_isolated_black[square] |= files_isolated[file] & rank_table[rank - 1];

    if (rank > rank_2)
      pawn_isolated_black[square] |= files_isolated[file] & rank_table[rank - 2];
    passed_pawn_connected[square] = pawn_isolated_white[square] | pawn_isolated_black[square]
      | (rank_table[rank] & files_isolated[file]);
  }

  for (rank = rank_1; rank <= rank_8; rank++) {
    in_front_white[rank] = 0;

    for (j = rank + 1; j <= rank_8; j++)
      in_front_white[rank] |= rank_table[j];
    not_in_front_white[rank] = ~in_front_white[rank];
  }

  for (rank = rank_8; rank >= rank_1; rank--) {
    in_front_black[rank] = 0;

    for (j = rank - 1; j >= rank_1; j--)
      in_front_black[rank] |= rank_table[j];
    not_in_front_black[rank] = ~in_front_black[rank];
  }

  for (u = 0; u < 128; u += 2)
    for (file = files_a; file <= files_h; file++) {
      T = 0;

      if (file < 7) {
        s = 1 << (file + 1);

        while (s < 256) {
          T |= s;

          if (u & s)
            break;

          s <<= 1;
        }
      }

      if (file > 0) {
        s = 1 << (file - 1);

        while (s > 0) {
          T |= s;

          if (u & s)
            break;

          s >>= 1;
        }
      }

      for (i = 0; i < 8; i++)
        bitboard_line_obscured[2][file + 8 * i][u >> 1] = T << 8 * i;
    }

  for (square = a1; square <= h8; square++) {
    passed_pawn_white[square] =
      (files_isolated[(square & 7)] | file_table[(square & 7)]) & in_front_white[(square >> 3)];
    passed_pawn_black[square] =
      (files_isolated[(square & 7)] | file_table[(square & 7)]) & in_front_black[(square >> 3)];
  }

  for (square = a1; square <= h8; square++) {
    if ((square & 7) >= files_c)
      west_two[square] = square_set[square - 2];
    else
      west_two[square] = 0;

    if (((square) & 7) <= files_f)
      east_two[square] = square_set[square + 2];
    else
      east_two[square] = 0;

    if ((square & 7) >= files_b)
      west_one[square] = square_set[square - 1];
    else
      west_one[square] = 0;

    if ((square & 7) <= files_g)
      east_one[square] = square_set[square + 1];
    else
      east_one[square] = 0;
    nudging[square] = west_one[square] | east_one[square];
  }

  for (square = a1; square <= h8; square++) {
    pawn_protected_white[square] = files_isolated[(square & 7)] & not_in_front_white[(square >> 3)];
    pawn_protected_black[square] = files_isolated[(square & 7)] & not_in_front_black[(square >> 3)];
  }

  for (square = a1; square <= h8; square++) {
    file = square & 7;
    rank = square >> 3;
    diagonal_length[square] = 0;

    if (file <= files_d) {
      while (file < files_h && rank < rank_8) {
        file++;
        rank++;
        diagonal_length[square] |= square_set[8 * rank + file];
      }
      file = square & 7;
      rank = square >> 3;

      while (file < files_h && rank > rank_1) {
        file++;
        rank--;
        diagonal_length[square] |= square_set[8 * rank + file];
      }
    }
    else {
      while (file > files_a && rank < rank_8) {
        file--;
        rank++;
        diagonal_length[square] |= square_set[8 * rank + file];
      }
      file = square & 7;
      rank = square >> 3;

      while (file > files_a && rank > rank_1) {
        file--;
        rank--;
        diagonal_length[square] |= square_set[8 * rank + file];
      }
    }
  }

  for (square = a1; square <= h8; square++)
    open_file_white[square] = file_table[(square & 7)] & in_front_white[(square >> 3)];

  for (square = a1; square <= h8; square++)
    open_file_black[square] = file_table[(square & 7)] & in_front_black[(square >> 3)];

  for (square = a1; square <= h8; square++)
    table_gain[square] = file_table[(square & 7)] ^ static_cast<uint64_t>(1) << square;

  for (square = a1; square <= h8; square++)
    for (i = 0; i < 64; i++) {
      T = bitboard_line_obscured[2][left_90[square]][i];
      bitboard_line_obscured[3][square][i] = 0;

      while (T) {
        b = bsf(T);
        bitboard_line_obscured[3][square][i] |= square_set[left_09[b]];
        T &= T - 1;
      }
    }

  for (u = 0; u < 128; u += 2)
    for (square = a1; square <= h8; square++) {
      T = 0;
      l = length[square];
      w = where[square];
      bitboard_line_obscured[1][right_54[square]][u >> 1] = 0;

      if (w < l) {
        s = 1 << (w + 1);

        while (s < 1 << l) {
          T |= s;

          if (u & s)
            break;

          s <<= 1;
        }
      }

      if (w > 0) {
        s = 1 << (w - 1);

        while (s > 0) {
          T |= s;

          if (u & s)
            break;

          s >>= 1;
        }
      }
      T <<= square - w;

      while (T) {
        b = bsf(T);
        bitboard_line_obscured[1][right_54[square]][u >> 1] |= square_set[right_54[b]];
        T &= T - 1;
      }
    }

  for (u = 0; u < 128; u += 2)
    for (square = a1; square <= h8; square++) {
      T = 0;
      l = length[square];
      w = where[square];
      bitboard_line_obscured[0][left_54[square]][u >> 1] = 0;

      if (w < l) {
        s = 1 << (w + 1);

        while (s < 1 << l) {
          T |= s;

          if (u & s)
            break;

          s <<= 1;
        }
      }

      if (w > 0) {
        s = 1 << (w - 1);

        while (s > 0) {
          T |= s;

          if (u & s)
            break;

          s >>= 1;
        }
      }
      T <<= square - w;

      while (T) {
        b = bsf(T);
        bitboard_line_obscured[0][left_54[square]][u >> 1] |= square_set[left_54[b]];
        T &= T - 1;
      }
    }

  for (square = a1; square <= h8; square++) {
    quadrant_black_wtm[square] = quadrant_black_btm[square] = 0;
    j = (square & 7) + 56;

    if (square >> 3 == rank_2)
      square_2 = square + 8;
    else
      square_2 = square;

    for (i = a1; i <= h8; i++) {
      if (DISTANCE(square_2, j) < DISTANCE(j, i) - 1)
        quadrant_black_btm[square] |= static_cast<uint64_t>(1) << i;

      if (DISTANCE(square_2, j) < DISTANCE(j, i))
        quadrant_black_wtm[square] |= static_cast<uint64_t>(1) << i;
    }
  }

  for (square = a1; square <= h8; square++) {
    quadrant_white_wtm[square] = quadrant_white_btm[square] = 0;
    j = square & 7;

    if (square >> 3 == rank_7)
      square_2 = square - 8;
    else
      square_2 = square;

    for (i = a1; i <= h8; i++) {
      if (DISTANCE(square_2, j) < DISTANCE(j, i) - 1)
        quadrant_white_wtm[square] |= static_cast<uint64_t>(1) << i;

      if (DISTANCE(square_2, j) < DISTANCE(j, i))
        quadrant_white_btm[square] |= static_cast<uint64_t>(1) << i;
    }
  }

  for (square = a1; square <= h8; square++) {
    shepherd_white[square] = shepherd_black[square] = 0;
    file = square & 7;

    if (file == files_a || file == files_h)
      T = files_isolated[file];
    else
      T = files_isolated[file] | file_table[file];

    if (square >> 3 >= rank_6)
      shepherd_white[square] |= T & 0xff00000000000000;

    if (square >> 3 >= rank_5)
      shepherd_white[square] |= T & 0x00ff000000000000;

    if (square >> 3 <= rank_3)
      shepherd_black[square] |= T & 0x00000000000000ff;

    if (square >> 3 <= rank_4)
      shepherd_black[square] |= T & 0x000000000000ff00;
  }

  for (square = a1; square <= h8; square++)
    for (king_square = a1; king_square <= h8; king_square++) {
      evasion_table[king_square][square] = attack_king[king_square];

      if (king_square >> 3 == square >> 3) {
        if ((king_square & 7) != files_a)
          evasion_table[king_square][square] ^= square_set[king_square - 1];

        if ((king_square & 7) != files_h)
          evasion_table[king_square][square] ^= square_set[king_square + 1];
      }

      if ((king_square & 7) == (square & 7)) {
        if (king_square >> 3 != rank_1)
          evasion_table[king_square][square] ^= square_set[king_square - 8];

        if (king_square >> 3 != rank_8)
          evasion_table[king_square][square] ^= square_set[king_square + 8];
      }

      if ((king_square >> 3) - (square >> 3) == (king_square & 7) - (square & 7)) {
        if (king_square >> 3 != rank_8 && (king_square & 7) != files_h)
          evasion_table[king_square][square] ^= square_set[king_square + 9];

        if (king_square >> 3 != rank_1 && (king_square & 7) != files_a)
          evasion_table[king_square][square] ^= square_set[king_square - 9];
      }

      if ((king_square >> 3) - (square >> 3) == (square & 7) - (king_square & 7)) {
        if (king_square >> 3 != rank_8 && (king_square & 7) != files_a)
          evasion_table[king_square][square] ^= square_set[king_square + 7];

        if (king_square >> 3 != rank_1 && (king_square & 7) != files_h)
          evasion_table[king_square][square] ^= square_set[king_square - 7];
      }

      if (attack_king[king_square] & square_set[square])
        evasion_table[king_square][square] |= square_set[square];
    }

  for (file = files_a; file <= files_h; file++) {
    files_left[file] = files_right[file] = 0;

    for (i = files_a; i < file; i++)
      files_left[file] |= file_table[i];

    for (i = file + 1; i <= files_h; i++)
      files_right[file] |= file_table[i];
  }

  for (square = a1; square <= h8; square++)
    for (king_square = a1; king_square <= h8; king_square++) {
      interposition_table[king_square][square] = square_set[square];
      dir = 0;

      if (king_square >> 3 == square >> 3) {
        if (king_square > square)
          dir = 1;
        else
          dir = -1;
      }

      if ((king_square & 7) == (square & 7)) {
        if (king_square > square)
          dir = 8;
        else
          dir = -8;
      }

      if ((king_square >> 3) - (square >> 3) == (king_square & 7) - (square & 7)) {
        if (king_square > square)
          dir = 9;
        else
          dir = -9;
      }

      if ((king_square >> 3) - (square >> 3) == (square & 7) - (king_square & 7)) {
        if (king_square > square)
          dir = 7;
        else
          dir = -7;
      }

      if (dir)
        for (i = square; i != king_square; i += dir)
          interposition_table[king_square][square] |= static_cast<uint64_t>(1) << i;
    }

  for (square = a1; square <= h8; square++) {
    orthogonal[square] = rank_table[(square >> 3)] | file_table[(square & 7)];
    diagonal[square] = 0;

    for (file = square & 7, rank = square >> 3;
         file <= files_h && rank <= rank_8; file++, rank++)
      diagonal[square] |= static_cast<uint64_t>(1) << (8 * rank + file);

    for (file = square & 7, rank = square >> 3;
         file <= files_h && rank >= rank_1; file++, rank--)
      diagonal[square] |= static_cast<uint64_t>(1) << (8 * rank + file);

    for (file = square & 7, rank = square >> 3;
         file >= files_a && rank <= rank_8; file--, rank++)
      diagonal[square] |= static_cast<uint64_t>(1) << (8 * rank + file);

    for (file = square & 7, rank = square >> 3;
         file >= files_a && rank >= rank_1; file--, rank--)
      diagonal[square] |= static_cast<uint64_t>(1) << (8 * rank + file);
    orthogonal[square] &= square_clear[square];
    diagonal[square] &= square_clear[square];
    non_orthogonal[square] = ~orthogonal[square];
    non_diagonal[square] = ~diagonal[square];
    diagonal_and_orthogonal[square] = orthogonal[square] | diagonal[square];
  }

  for (j = a1; j <= h8; j++)
    for (i = a1; i <= h8; i++) {
      direction[i][j] = 37;

      if (i == j)
        continue;

      if (j >> 3 == i >> 3)
        direction[i][j] = 2;

      if ((j & 7) == (i & 7))
        direction[i][j] = 3;

      if ((i & 7) - (j & 7) == (i >> 3) - (j >> 3))
        direction[i][j] = 1;

      if ((j & 7) - (i & 7) == (i >> 3) - (j >> 3))
        direction[i][j] = 0;
    }
  init_pawns();
  init_rand_hash();
}

void age_increase() {
  age += 1;

  if (age == 256)
    age = 0;
}

void clear_hash() {
  uint32_t target;
  const uint32_t size = hash_size * 1024 * 1024 / sizeof(type_hash);

  for (target = 1; target <= size; target *= 2) {
  }

  if (target > size)
    target /= 2;
  memset(hash_table, 0, target * sizeof(type_hash));
  memset(pv_hash_table, 0, 0x10000 * sizeof(type_pv_hash));
  age = 0;
}

void init_hash() {
  uint32_t target;
  const uint32_t size = hash_size * 1024 * 1024 / sizeof(type_hash);

  for (target = 1; target <= size; target *= 2) {
  }

  if (target > size)
    target /= 2;
  age = 0;
  hash_mask = target - 1 & 0xfffffffc;

  if (hash_table != nullptr)
    free(hash_table);
  hash_table = static_cast<type_hash*>(malloc(target * sizeof(type_hash)));

  clear_hash();
}

void hash_low_all(int move, const int depth, const int score) {
  const int k = position->hash_key & hash_mask;
  type_hash* hash;
  int max = 0, w = 0;
  move &= 0x7fff;

  for (int i = 0; i < 4; i++) {
    hash = hash_table + (k + i);

    if ((hash->hash_key ^ position->hash_key >> 32) == 0 && (!hash->depth_low || hash->flag & 8)
      && hash->depth_low <= depth) {
      hash->depth_low = depth;
      hash->move = move;
      hash->score_low = score;
      hash->age = age;
      hash->flag |= 1 | 8;
      return;
    }

    if (const int deepness = hash->depth_low >= hash->depth_high ? hash->depth_low : hash->depth_high; (hash->age - age
      & 256 - 1) * 256 + (256 - deepness) > max) {
      max = (hash->age - age & 256 - 1) * 256 + (256 - deepness);
      w = i;
    }
  }
  hash = hash_table + (k + w);
  hash->hash_key = position->hash_key >> 32;
  hash->depth_high = 0;
  hash->score_high = 0;
  hash->depth_low = depth;
  hash->move = move;
  hash->score_low = score;
  hash->age = age;
  hash->flag = 1 | 8;
}

void hash_high_cut(const int depth, const int score) {
  const int k = position->hash_key & hash_mask;
  type_hash* hash;
  int max = 0, w = 0;

  for (int i = 0; i < 4; i++) {
    hash = hash_table + (k + i);

    if (!(hash->hash_key ^ position->hash_key >> 32) && (!hash->depth_high || hash->flag & 4)
      && hash->depth_high <= depth) {
      hash->depth_high = depth;
      hash->score_high = score;
      hash->age = age;
      hash->flag |= 2 | 4;
      return;
    }

    if (const int deepness = hash->depth_low >= hash->depth_high ? hash->depth_low : hash->depth_high; (hash->age - age
      & 256 - 1) * 256 + (256 - deepness) > max) {
      max = static_cast<int>((hash->age - age & 256 - 1) * 256 + (256 - deepness));
      w = i;
    }
  }
  hash = hash_table + (k + w);
  hash->hash_key = position->hash_key >> 32;
  hash->depth_low = 0;
  hash->move = 0;
  hash->score_low = 0;
  hash->depth_high = depth;
  hash->score_high = score;
  hash->age = age;
  hash->flag = 2 | 4;
}

void hash_low(const uint64_t hash_key, int move, const int depth, const int score) {
  const int k = static_cast<int>(hash_key & hash_mask);
  type_hash* hash;
  int max = 0, w = 0;
  move &= 0x7fff;

  for (int i = 0; i < 4; i++) {
    hash = hash_table + (k + i);

    if (!(hash->hash_key ^ hash_key >> 32) && !(hash->flag & 16) && hash->depth_low <= depth) {
      hash->depth_low = static_cast<uint8_t>(depth);
      hash->move = static_cast<uint16_t>(move);
      hash->score_low = static_cast<int16_t>(score);
      hash->age = static_cast<uint8_t>(age);
      hash->flag |= 1;
      hash->flag &= ~8;
      return;
    }

    if (const int deepness = hash->depth_low >= hash->depth_high ? hash->depth_low : hash->depth_high; (hash->age - age
      & 256 - 1) * 256 + (256 - deepness) > max) {
      max = (hash->age - age & 256 - 1) * 256 + (256 - deepness);
      w = i;
    }
  }
  hash = hash_table + (k + w);
  hash->hash_key = hash_key >> 32;
  hash->depth_high = 0;
  hash->score_high = 0;
  hash->depth_low = depth;
  hash->move = move;
  hash->score_low = score;
  hash->age = age;
  hash->flag = 1;
}

void hash_high(const uint64_t hash_key, const int depth, const int score) {
  const int k = hash_key & hash_mask;
  type_hash* hash;
  int max = 0, w = 0;

  for (int i = 0; i < 4; i++) {
    hash = hash_table + (k + i);

    if (!(hash->hash_key ^ hash_key >> 32) && !(hash->flag & 16) && hash->depth_high <= depth) {
      hash->depth_high = depth;
      hash->score_high = score;
      hash->age = age;
      hash->flag |= 2;
      hash->flag &= ~4;
      return;
    }

    if (const int deepness = hash->depth_low >= hash->depth_high ? hash->depth_low : hash->depth_high; (hash->age - age
      & 256 - 1) * 256 + (256 - deepness) > max) {
      max = (hash->age - age & 256 - 1) * 256 + (256 - deepness);
      w = i;
    }
  }
  hash = hash_table + (k + w);
  hash->hash_key = hash_key >> 32;
  hash->depth_low = 0;
  hash->move = 0;
  hash->score_low = 0;
  hash->depth_high = depth;
  hash->score_high = score;
  hash->age = age;
  hash->flag = 2;
}

void pv_hash(const int move, const int depth, const int score) {
  const int k = position->hash_key & 0xfffc;
  type_pv_hash* hash;
  int w = 0, max = 0;

  for (int i = 0; i < 4; i++) {
    hash = pv_hash_table + (k + i);

    if (hash->hash_key == position->hash_key) {
      hash->depth = depth;
      hash->score = score;
      hash->move = move;
      hash->age = age;
      return;
    }

    if ((hash->age - age & 256 - 1) * 256 + (256 - hash->depth) > max) {
      max = (hash->age - age & 256 - 1) * 256 + (256 - hash->depth);
      w = i;
    }
  }
  hash = pv_hash_table + (k + w);
  hash->hash_key = position->hash_key;
  hash->depth = depth;
  hash->move = move;
  hash->score = score;
  hash->age = age;
}

void hash_exact(int move, const int depth, const int score, const int fl) {
  const int k = position->hash_key & hash_mask;
  type_hash* hash;
  int max = 0, w = 0;
  move &= 0x7fff;
  pv_hash(move, depth, score);

  for (int i = 0; i < 4; i++) {
    hash = hash_table + (k + i);

    if ((hash->hash_key ^ position->hash_key >> 32) == 0
      && (hash->depth_high >= hash->depth_low ? hash->depth_high : hash->depth_low)
      <= depth) {
      hash->depth_high = hash->depth_low = depth;
      hash->move = move;
      hash->score_high = hash->score_low = score;
      hash->age = age;
      hash->flag = fl;

      for (int j = i + 1; j < 4; j++) {
        hash = hash_table + (k + j);

        if ((hash->hash_key ^ position->hash_key >> 32) == 0
          && (hash->depth_high >= hash->depth_low
                ? hash->depth_high
                : hash->depth_low) <= depth) {
          memset(hash, 0, 16);
          hash->age = age ^ 256 / 2;
        }
      }
      return;
    }

    if (const int deepness = hash->depth_low >= hash->depth_high ? hash->depth_low : hash->depth_high; (hash->age - age
      & 256 - 1) * 256 + (256 - deepness) > max) {
      max = (hash->age - age & 256 - 1) * 256 + (256 - deepness);
      w = i;
    }
  }
  hash = hash_table + (k + w);
  hash->hash_key = position->hash_key >> 32;
  hash->depth_high = hash->depth_low = depth;
  hash->move = move;
  hash->score_high = hash->score_low = score;
  hash->age = age;
  hash->flag = fl;
}

void init_pawn_hash_key() {
  pawn_hash_table = static_cast<type_pawn_hash*>(malloc((1 << 16) * sizeof(type_pawn_hash)));
  if (pawn_hash_table != nullptr)
    memset(pawn_hash_table, 0, (1 << 16) * sizeof(type_pawn_hash));
}

void clear_gain() {
  for (auto& j : max_increase)
    for (short& i : j)
      i = 0;
}

void clear_history() {
  for (auto& piece : history_table)
    for (int square = a1; square <= h8; square++)
      piece[square] = 0x800;
}

void uci() {
  printf("id name %s %s %s\n", ENGINE, VERSION, PLATFORM);
  printf("id author %s\n", AUTHOR);
  printf("option name Hash type spin default 256 min 4 max 4096\n");
  printf("uciok\n");
  fflush(stdout);
}

void readyok() {
  printf("readyok\n");
  fflush(stdout);
}

void parse(char* string) {
  if (!strcmp(string, "ponderhit")) {
    if (move_easy)
      halt_search();
    else
      absolute_time -= get_time() - clock_start;
  }
  if (!strcmp(string, "quit"))
    exit(1);

  if (!strcmp(string, "stop"))
    halt_search();

  if (!strcmp(string, "isready"))
    readyok();

  if (!strcmp(string, "ucinewgame")) {
    halt_search();
    init_game();
  }

  if (!memcmp(string, "setoption", 9)) {
    halt_search();
    const char* name = strstr(string, "name ");
    char* value = strstr(string, "value ");

    if (name == nullptr || value == nullptr || name >= value)
      return;
    value[-1] = 0;
    name += 5;
    value += 6;

    if (!memcmp(name, "Hash", 4)) {
      hash_size = atoi(value);

      if (hash_size >= 4 && hash_size <= 2048) { init_hash(); }
    }
  }

  if (jump_ok)
    return;

  if (!memcmp(string, "go", 2)) {
    setup_search(string);
    search();
  }

  if (!memcmp(string, "position", 8))
    init_position(string + 9);

  if (!strcmp(string, "uci"))
    uci();
}

void engine_info() {
  const auto startup_banner = "" ENGINE " " VERSION " " PLATFORM "\n\n";
  printf(startup_banner);
}

void read_input() {
  char string[2048];
  fgets(string, 2048, stdin);
  string[strlen(string) - 1] = 0;
  parse(string);
}

void halt_search() {
  stop_flag = 1;

  if (jump_ok)
    longjmp(jbuf, 1);
}

void send_info(const uint64_t x) {
  uint64_t speed;
  const uint64_t nodes = nodes_white + nodes_black + nodes_null;

  previous_info = x;
  const uint64_t time = x / 1000;

  if (time == 0)
    speed = 0;
  else
    speed = nodes / time * 1000;

  printf("info time %llu nodes %llu nps %llu\n", time, nodes, speed);
  fflush(stdout);
}

void check_if_done(const int g) {
  if (!jump_ok)
    return;

  const int64_t x = get_time() - clock_start;

  if (g == depth_limit)
    halt_search();

  if (x - previous_info > 1000000)
    send_info(x);

  if (g >= 1 && g < 8)
    goto END;

  if (x > absolute_time)
    halt_search();

  if (g == 0)
    goto END;

  if (!bad_move && x >= battle_time)
    halt_search();

  if (move_easy && x >= easy_time)
    halt_search();

  if (!move_battle && x >= normal_time && !bad_move)
    halt_search();

END:
  while (get_input())
    read_input();
}

void setup_search(char* string) {
  int64_t white_time = 0xfffffffffffffff, black_time = 0xfffffffffffffff, white_increment = 0, black_increment = 0;

  depth_limit = 255;
  absolute_time = trouble_time = 0xfffffffffffffff;
  stop_flag = 0;
  previous_info = 0;
  const char* p = strtok(string, " ");

  for (p = strtok(nullptr, " "); p != nullptr; p = strtok(nullptr, " ")) {
    if (!strcmp(p, "depth")) {
      p = strtok(nullptr, " ");
      depth_limit = 1 >= atoi(p) ? 1 : atoi(p);
    }
    else if (!strcmp(p, "movetime")) {
      p = strtok(nullptr, " ");
      absolute_time = (1 >= atoi(p) ? 1 : atoi(p)) * 1000;
    }
    else if (!strcmp(p, "wtime")) {
      p = strtok(nullptr, " ");
      white_time = _atoi64(p) * 1000;
    }
    else if (!strcmp(p, "btime")) {
      p = strtok(nullptr, " ");
      black_time = _atoi64(p) * 1000;
    }
    else if (!strcmp(p, "winc")) {
      p = strtok(nullptr, " ");
      white_increment = _atoi64(p) * 1000;
    }
    else if (!strcmp(p, "binc")) {
      p = strtok(nullptr, " ");
      black_increment = _atoi64(p) * 1000;
    }
    else if (!strcmp(p, "ponder") || !strcmp(p, "infinite")) {
    }
  }

  battle_time = 0xfffffffffffffff;
  normal_time = 0xfffffffffffffff;
  easy_time = 0xfffffffffffffff;
  const int64_t temp = board.white_to_move ? white_time : black_time;

  if (temp == 0xfffffffffffffff)
    goto END;

  increment_time = board.white_to_move ? white_increment : black_increment;
  absolute_time = temp / 2 - 10000;

  if (absolute_time < 5000)
    absolute_time = 5000;

  trouble_time = absolute_time / 22 + increment_time;

  if (trouble_time > absolute_time)
    trouble_time = absolute_time;

  if (trouble_time < 5000)
    trouble_time = 5000;

  easy_time = trouble_time / 4;
  battle_time = trouble_time;
  normal_time = 3 * trouble_time / 4;

END:
  if (temp == 0xfffffffffffffff)
    analysis_mode = 1;
  else
    analysis_mode = 0;
}

void output_move() {
  if (!best_move) {
    printf("bestmove NULL\n");
    fflush(stdout);
    return;
  }

  printf("bestmove %s ponder %s\n", notate(best_move, string_1), notate(ponder_move, string_2));
  fflush(stdout);
}

char* cp_or_mate(const int score, char* s) {
  if (score > 30000 - 1024)
    sprintf(s, "mate %d", (30000 + 1 - score) / 2);

  else if (score < -30000 + 1024)
    sprintf(s, "mate %d", (-30000 - score) / 2);

  else
    sprintf(s, "cp %d", score);
  return s;
}

void information(const uint64_t x, const int score) {
  int64_t speed;
  const int64_t nodes = nodes_white + nodes_black + nodes_null;
  int i;
  type_position* p;
  char pv[1536];
  uint64_t hash_stack[256];
  int count = 0;
  int pv_move = 0;

  memset(hash_stack, 0, 256 * sizeof(uint64_t));

  const int64_t time = x / 1000;
  if (time == 0)
    speed = 0;
  else
    speed = nodes / time * 1000;

  char* q = pv;
  hash_stack[count++] = position->hash_key;
  notate(best_move, string_1);
  strcpy(q, string_1);
  q += strlen(string_1);
  strcpy(q, " ");
  q++;
  int move = best_move;

  while (move) {
    make(move);

    pv_move++;
    if (pv_move == 2)
      ponder_move = move;

    eval(-0x7fff0000, 0x7fff0000, 0);
    uint8_t B = 0;

    for (i = 0; i < count; i++)
      if (hash_stack[i] == position->hash_key)
        B = 1;

    if (B)
      break;
    hash_stack[count++] = position->hash_key;
    move = 0;
    const int k = position->hash_key & 0xfffc;

    for (i = 0; i < 4; i++) {
      if (const type_pv_hash* hash = pv_hash_table + (k + i); hash->hash_key == position->hash_key) {
        move = hash->move;
        break;
      }
    }

    if (!move || (board.white_to_move ? !white_ok(move) : !black_ok(move)))
      break;

    if (count > 250)
      break;
    notate(move, string_1);
    strcpy(q, string_1);
    q += strlen(string_1);
    strcpy(q, " ");
    q++;
  }

  q--;
  *q = 0;

  while (position != root_position + 1) {
    if (!position->move)
      undo_null();
    else
      undo(position->move);
  }

  printf("info time %llu nodes %llu nps %llu score %s depth %d pv %s\n",
         time, nodes, speed, cp_or_mate(score, string_2), best_depth / 2, pv);
  fflush(stdout); // NOLINT(cert-err33-c)
}

void search() {
  type_position* p;
  new_game = 0;
  clock_start = get_time();
  stack_height = -1;
  best_move = best_depth = best_score = 0;
  nodes_white = nodes_black = nodes_null = 0;

  for (p = root_position; p <= position; p++)
    stack[++stack_height] = p->hash_key;

  if (analysis_mode) {
    for (p = root_position; p < position; p++) {
      uint8_t repetition = 0;

      for (const type_position* q = p + 2; q < position; q += 2)
        if (p->hash_key == q->hash_key) {
          repetition = 1;
          break;
        }

      if (!repetition)
        stack[p - root_position] = 0;
      (p + 1)->move = 0;
    }
  }
  memcpy(root_position + 1, position, sizeof(type_position));
  memset(root_position + 2, 0, 254 * sizeof(type_position));
  memset(root_position, 0, sizeof(type_position));
  position = root_position + 1;
  age_increase();
  best_score_previous = -30000;
  move_easy = 0;
  jump_ok = 1;

  if (const int z = setjmp(jbuf); !z) {
    if (board.white_to_move)
      white_top();
    else
      black_top();
  }
  jump_ok = 0;
  previous_depth = best_depth;

  while (position != root_position + 1) {
    if (!position->move)
      undo_null();
    else
      undo(position->move);
  }
  information(get_time() - clock_start, best_score);
  output_move();
}

int init_flags(const int white_pawn_count, const int white_knight_count, const int white_bishop_count,
               const int white_bishop_count_1,
               const int white_bishop_count_2, const int white_rook_count, const int white_queen_count,
               const int black_pawn_count, const int black_knight_count,
               const int black_bishop_count, const int black_bishop_count_1, const int black_bishop_count_2,
               const int black_rook_count, const int black_queen_count) {
  uint8_t flags = (white_knight_count || white_bishop_count || white_queen_count || white_rook_count) << 1
    | (black_knight_count || black_bishop_count || black_queen_count || black_rook_count) << 0;

  if (white_queen_count == 1 && black_queen_count == 1 && !white_rook_count && !black_rook_count
    && !white_bishop_count && !black_bishop_count && !white_knight_count && !black_knight_count)
    flags |= 1 << 2;

  if (white_rook_count == 1 && black_rook_count == 1 && !white_queen_count && !black_queen_count
    && !white_bishop_count && !black_bishop_count && !white_knight_count && !black_knight_count)
    flags |= 2 << 2;

  if (white_bishop_count == 1 && black_bishop_count == 1 && !white_queen_count && !black_queen_count
    && !white_rook_count && !black_rook_count && !white_knight_count && !black_knight_count) {
    if ((white_bishop_count_1 == 1 && black_bishop_count_2 == 1) || (white_bishop_count_2 == 1 &&
      black_bishop_count_1 == 1))
      flags |= 4 << 2;
    else
      flags |= 3 << 2;
    flags |= (8 | 16) << 2;
  }

  if (white_knight_count == 1 && black_knight_count == 1 && !white_queen_count && !black_queen_count
    && !white_rook_count && !black_rook_count && !white_bishop_count && !black_bishop_count)
    flags |= 5 << 2;

  if (white_knight_count == 1 && black_bishop_count == 1 && !white_queen_count && !black_queen_count
    && !white_rook_count && !black_rook_count && !white_bishop_count && !black_knight_count)
    flags |= 6 << 2;

  if (white_bishop_count == 1 && black_knight_count == 1 && !white_queen_count && !black_queen_count
    && !white_rook_count && !black_rook_count && !black_bishop_count && !white_knight_count)
    flags |= 6 << 2;

  if (white_bishop_count == 1 && !white_queen_count && !white_rook_count && !white_knight_count)
    flags |= 8 << 2;

  if (black_bishop_count == 1 && !black_queen_count && !black_rook_count && !black_knight_count)
    flags |= 16 << 2;

  if (white_knight_count == 1 && !white_queen_count && !white_rook_count && !white_bishop_count)
    flags |= 8 << 2;

  if (black_knight_count == 1 && !black_queen_count && !black_rook_count && !black_bishop_count)
    flags |= 16 << 2;

  if (!white_knight_count && !white_bishop_count && !white_rook_count && !white_queen_count && !black_knight_count
    && !black_bishop_count && !black_queen_count && !black_queen_count && (white_pawn_count + black_pawn_count == 1))
    flags |= 7 << 2;

  if (white_knight_count == 1 && white_bishop_count == 1 && !white_rook_count && !white_queen_count
    && !white_pawn_count && !black_queen_count && !black_rook_count && !black_bishop_count && !black_knight_count
    && !black_pawn_count)
    flags |= 32 << 2;

  if (black_knight_count == 1 && black_bishop_count == 1 && !black_rook_count && !black_queen_count
    && !black_pawn_count && !white_queen_count && !white_rook_count && !white_bishop_count && !white_knight_count
    && !white_pawn_count)
    flags |= 32 << 2;
  return flags;
}

int init_weights(const int white_pawn_count, const int white_knight_count, const int white_bishop_count,
                 const int white_bishop_count_1,
                 const int white_bishop_count_2, const int white_rook_count, const int white_queen_count,
                 const int black_pawn_count, const int black_knight_count,
                 const int black_bishop_count, const int black_bishop_count_1, const int black_bishop_count_2,
                 const int black_rook_count, const int black_queen_count) {
  int token = 0x80;

  if (white_knight_count == 0 && black_knight_count == 0 && white_bishop_count == 0 && black_bishop_count == 0
    && white_rook_count == 0 && black_rook_count == 0 && white_queen_count == 1 && black_queen_count == 1)
    token = 0x70 + (white_pawn_count >= black_pawn_count ? white_pawn_count : black_pawn_count);

  if (white_knight_count == 0 && black_knight_count == 0 && white_bishop_count == 0 && black_bishop_count == 0
    && white_queen_count == 0 && black_queen_count == 0 && white_rook_count == 1 && black_rook_count == 1)
    token =
      0x60 + 2 * (white_pawn_count >= black_pawn_count ? white_pawn_count : black_pawn_count);

  if (white_knight_count == 0 && black_knight_count == 0 && white_rook_count == 0 && black_rook_count == 0
    && white_queen_count == 0 && black_queen_count == 0 && white_bishop_count == 1 && black_bishop_count == 1) {
    if ((white_bishop_count_1 == 1 && white_bishop_count_2 == 0 && black_bishop_count_1 == 0 && black_bishop_count_2
        == 1)
      || (white_bishop_count_1 == 0 && white_bishop_count_2 == 1 && black_bishop_count_1 == 1 &&
        black_bishop_count_2 == 0))
      token = 0x30 + 4 * (white_pawn_count >= black_pawn_count ? white_pawn_count : black_pawn_count);
    else
      token = 0x78 + 2 * (white_pawn_count >= black_pawn_count ? white_pawn_count : black_pawn_count);
  }

  if (white_knight_count == 1 && black_knight_count == 1 && white_rook_count == 0 && black_rook_count == 0
    && white_queen_count == 0 && black_queen_count == 0 && white_bishop_count == 0 && black_bishop_count == 0)
    token = 0x80 + (white_pawn_count >= black_pawn_count ? white_pawn_count : black_pawn_count);

  if (white_knight_count == 0 && black_knight_count == 0 && white_rook_count == 0 && black_rook_count == 0
    && white_queen_count == 0 && black_queen_count == 0 && white_bishop_count == 0 && black_bishop_count == 0)
    token = 0xc0 - 8 * (white_pawn_count >= black_pawn_count ? white_pawn_count : black_pawn_count);

  if (white_knight_count == 0 && black_knight_count == 0 && white_bishop_count == 1 && black_bishop_count == 1
    && white_queen_count == 0 && black_queen_count == 0 && white_rook_count == 1 && black_rook_count == 1) {
    if ((white_bishop_count_1 == 1 && white_bishop_count_2 == 0 && black_bishop_count_1 == 0 && black_bishop_count_2
        == 1)
      || (white_bishop_count_1 == 0 && white_bishop_count_2 == 1 && black_bishop_count_1 == 1 &&
        black_bishop_count_2 == 0))
      token = 0x70 + (white_pawn_count >= black_pawn_count ? white_pawn_count : black_pawn_count);
  }
  return token;
}

int calc_white_weights(int white_pawn_count, int white_knight_count, int white_bishop_count,
                       int white_bishop_count_1,
                       int white_bishop_count_2, int white_rook_count, int white_queen_count,
                       int black_pawn_count, int black_knight_count,
                       int black_bishop_count, int black_bishop_count_1, int black_bishop_count_2,
                       int black_rook_count, int black_queen_count) {
  int white_minor_count, black_minor_count, white_phase, black_phase, white_weight, white_value, black_value;
  white_minor_count = white_bishop_count + white_knight_count;
  black_minor_count = black_bishop_count + black_knight_count;
  white_phase = white_minor_count + 2 * white_rook_count + 4 * white_queen_count;
  black_phase = black_minor_count + 2 * black_rook_count + 4 * black_queen_count;
  white_value = 3 * (white_bishop_count + white_knight_count) + 5 * white_rook_count + 9 * white_queen_count;
  black_value = 3 * (black_bishop_count + black_knight_count) + 5 * black_rook_count + 9 * black_queen_count;
  white_weight = 10;

  if (!white_pawn_count) {
    if (white_phase == 1)
      white_weight = 0;

    if (white_phase == 2) {
      if (black_phase == 0) {
        if (white_knight_count == 2) {
          if (black_pawn_count >= 1)
            white_weight = 3;
          else
            white_weight = 0;
        }
      }

      if (black_phase == 1) {
        white_weight = 1;

        if (white_bishop_count == 2 && black_knight_count == 1)
          white_weight = 8;

        if (white_rook_count == 1 && black_knight_count == 1)
          white_weight = 2;
      }

      if (black_phase == 2)
        white_weight = 1;
    }

    if (white_phase == 3 && white_rook_count == 1) {
      if (black_phase == 2 && black_rook_count == 1) {
        if (white_knight_count == 1)
          white_weight = 1;

        if (white_bishop_count == 1)
          white_weight = 1;
      }

      if (black_phase == 2 && black_rook_count == 0) {
        white_weight = 2;

        if (white_bishop_count == 1 && black_knight_count == 2)
          white_weight = 6;

        if (black_knight_count == 1 && ((white_bishop_count_1 == 1 && black_bishop_count_1 == 1)
          || (white_bishop_count_2 == 1 && black_bishop_count_2 == 1)))
          white_weight = 2;

        if (black_knight_count == 1 && ((white_bishop_count_2 == 1 && black_bishop_count_1 == 1)
          || (white_bishop_count_1 == 1 && black_bishop_count_2 == 1)))
          white_weight = 7;
      }

      if (black_phase == 3)
        white_weight = 2;
    }

    if (white_phase == 3 && white_rook_count == 0) {
      if (black_phase == 2 && black_rook_count == 1) {
        if (white_knight_count == 2)
          white_weight = 2;

        if (white_bishop_count == 2)
          white_weight = 7;
      }

      if (black_phase == 2 && black_rook_count == 0) {
        white_weight = 2;

        if (white_bishop_count == 2 && black_knight_count == 2)
          white_weight = 4;
      }

      if (black_phase == 3)
        white_weight = 2;
    }

    if (white_phase == 4 && white_queen_count) {
      if (black_phase == 2 && black_knight_count == 2)
        white_weight = 2;

      if (black_phase == 2 && black_knight_count == 1)
        white_weight = 8;

      if (black_phase == 2 && black_knight_count == 0)
        white_weight = 7;

      if (black_phase == 3)
        white_weight = 1;

      if (black_phase == 4)
        white_weight = 1;
    }

    if (white_phase == 4 && white_rook_count == 2) {
      if (black_phase == 2 && black_rook_count == 0)
        white_weight = 7;

      if (black_phase == 3)
        white_weight = 2;

      if (black_phase == 4)
        white_weight = 1;
    }

    if (white_phase == 4 && white_rook_count == 1) {
      if (black_phase == 3 && black_rook_count == 1)
        white_weight = 3;

      if (black_phase == 3 && black_rook_count == 0)
        white_weight = 2;

      if (black_phase == 4)
        white_weight = 2;
    }

    if (white_phase == 4 && white_rook_count == 0 && white_queen_count == 0) {
      if (black_phase == 3 && black_rook_count == 1)
        white_weight = 4;

      if (black_phase == 3 && black_rook_count == 0)
        white_weight = 2;

      if (black_phase == 4 && black_queen_count)
        white_weight = 8;

      if (black_phase == 4 && black_queen_count == 0)
        white_weight = 1;
    }

    if (white_phase == 5 && white_queen_count) {
      if (black_phase == 4)
        white_weight = 2;

      if (black_phase == 5)
        white_weight = 1;

      if (black_phase == 4 && black_rook_count == 2) {
        if (white_knight_count)
          white_weight = 3;

        if (white_bishop_count)
          white_weight = 7;
      }

      if (black_phase == 5)
        white_weight = 1;
    }

    if (white_phase == 5 && white_rook_count == 1) {
      if (black_phase == 4 && black_queen_count)
        white_weight = 9;

      if (black_phase == 4 && black_rook_count == 2)
        white_weight = 7;

      if (black_phase == 4 && black_rook_count == 1)
        white_weight = 3;

      if (black_phase == 4 && black_queen_count == 0 && black_rook_count == 0)
        white_weight = 1;

      if (black_phase == 5)
        white_weight = 2;
    }

    if (white_phase == 5 && white_rook_count == 2) {
      if (black_phase == 4 && black_queen_count && white_bishop_count == 1)
        white_weight = 8;

      if (black_phase == 4 && black_queen_count && white_knight_count == 1)
        white_weight = 7;

      if (black_phase == 4 && black_rook_count == 2)
        white_weight = 3;

      if (black_phase == 4 && black_rook_count == 1)
        white_weight = 2;

      if (black_phase == 4 && black_queen_count == 0 && black_rook_count == 0)
        white_weight = 1;

      if (black_phase == 5)
        white_weight = 1;
    }

    if (white_phase == 6 && white_queen_count && white_rook_count) {
      if (black_phase == 4 && black_queen_count == 0 && black_rook_count == 0)
        white_weight = 2;

      if (black_phase == 5 && black_queen_count)
        white_weight = 1;

      if (black_phase == 4 && black_rook_count == 1)
        white_weight = 6;

      if (black_phase == 4 && black_rook_count == 2)
        white_weight = 3;

      if (black_phase == 5 && black_rook_count)
        white_weight = 1;

      if (black_phase == 6)
        white_weight = 1;
    }

    if (white_phase == 6 && white_queen_count && white_rook_count == 0) {
      if (black_phase == 4 && black_queen_count == 0 && black_rook_count == 0)
        white_weight = 5;

      if (black_phase == 5 && black_queen_count)
        white_weight = 2;

      if (black_phase == 5 && black_rook_count == 2)
        white_weight = 2;

      if (black_phase == 5 && black_rook_count == 1)
        white_weight = 1;

      if (black_phase == 6)
        white_weight = 1;
    }

    if (white_phase == 6 && white_queen_count == 0 && white_rook_count == 2) {
      if (black_phase == 5 && black_queen_count)
        white_weight = 7;

      if (black_phase == 5 && black_rook_count == 1)
        white_weight = 1;

      if (black_phase == 5 && black_rook_count == 2)
        white_weight = 2;

      if (black_phase == 6)
        white_weight = 1;
    }

    if (white_phase == 6 && white_queen_count == 0 && white_rook_count == 1) {
      if (black_phase == 5 && black_queen_count)
        white_weight = 9;

      if (black_phase == 5 && black_rook_count == 2)
        white_weight = 3;

      if (black_phase == 5 && black_rook_count == 1)
        white_weight = 2;

      if (black_phase == 6)
        white_weight = 1;

      if (black_phase == 6 && black_queen_count)
        white_weight = 2;

      if (black_phase == 6 && black_queen_count && black_rook_count)
        white_weight = 4;
    }

    if (white_phase >= 7) {
      if (white_value > black_value + 4)
        white_weight = 9;

      if (white_value == black_value + 4)
        white_weight = 7;

      if (white_value == black_value + 3)
        white_weight = 4;

      if (white_value == black_value + 2)
        white_weight = 2;

      if (white_value < black_value + 2)
        white_weight = 1;
    }
  }

  if (white_pawn_count == 1) {
    if (black_phase == 1) {
      if (white_phase == 1)
        white_weight = 3;

      if (white_phase == 2 && white_knight_count == 2) {
        if (black_pawn_count == 0)
          white_weight = 3;
        else
          white_weight = 5;
      }

      if (white_phase == 2 && white_rook_count == 1)
        white_weight = 7;
    }

    if (black_phase == 2 && black_rook_count == 1 && white_phase == 2 && white_rook_count == 1)
      white_weight = 8;

    if (black_phase == 2 && black_rook_count == 0 && white_phase == 2)
      white_weight = 4;

    if (black_phase >= 3 && black_minor_count > 0 && white_phase == black_phase)
      white_weight = 3;

    if (black_phase >= 3 && black_minor_count == 0 && white_phase == black_phase)
      white_weight = 5;

    if (black_phase == 4 && black_queen_count == 1 && white_phase == black_phase)
      white_weight = 7;
  }
  return white_weight;
}

int calc_black_weights(int white_pawn_count, int white_knight_count, int white_bishop_count,
                       int white_bishop_count_1,
                       int white_bishop_count_2, int white_rook_count, int white_queen_count,
                       int black_pawn_count, int black_knight_count,
                       int black_bishop_count, int black_bishop_count_1, int black_bishop_count_2,
                       int black_rook_count, int black_queen_count) {
  int white_minor_count, black_minor_count, white_phase, black_phase, black_weight, white_value, black_value;
  white_minor_count = white_bishop_count + white_knight_count;
  black_minor_count = black_bishop_count + black_knight_count;
  white_phase = white_minor_count + 2 * white_rook_count + 4 * white_queen_count;
  black_phase = black_minor_count + 2 * black_rook_count + 4 * black_queen_count;
  white_value = 3 * (white_bishop_count + white_knight_count) + 5 * white_rook_count + 9 * white_queen_count;
  black_value = 3 * (black_bishop_count + black_knight_count) + 5 * black_rook_count + 9 * black_queen_count;
  black_weight = 10;

  if (!black_pawn_count) {
    if (black_phase == 1)
      black_weight = 0;

    if (black_phase == 2) {
      if (white_phase == 0) {
        if (black_knight_count == 2) {
          if (black_pawn_count >= 1)
            black_weight = 3;
          else
            black_weight = 0;
        }
      }

      if (white_phase == 1) {
        black_weight = 1;

        if (black_bishop_count == 2 && white_knight_count == 1)
          black_weight = 8;

        if (black_rook_count == 1 && white_knight_count == 1)
          black_weight = 2;
      }

      if (white_phase == 2)
        black_weight = 1;
    }

    if (black_phase == 3 && black_rook_count == 1) {
      if (white_phase == 2 && white_rook_count == 1) {
        if (black_knight_count == 1)
          black_weight = 1;

        if (black_bishop_count == 1)
          black_weight = 1;
      }

      if (white_phase == 2 && white_rook_count == 0) {
        black_weight = 2;

        if (black_bishop_count == 1 && white_knight_count == 2)
          black_weight = 6;

        if (white_knight_count == 1 && ((black_bishop_count_1 == 1 && white_bishop_count_1 == 1)
          || (black_bishop_count_2 == 1 && white_bishop_count_2 == 1)))
          black_weight = 2;

        if (white_knight_count == 1 && ((black_bishop_count_2 == 1 && white_bishop_count_1 == 1)
          || (black_bishop_count_1 == 1 && white_bishop_count_2 == 1)))
          black_weight = 7;
      }

      if (white_phase == 3)
        black_weight = 2;
    }

    if (black_phase == 3 && black_rook_count == 0) {
      if (white_phase == 2 && white_rook_count == 1) {
        if (black_knight_count == 2)
          black_weight = 2;

        if (black_bishop_count == 2)
          black_weight = 7;
      }

      if (white_phase == 2 && white_rook_count == 0) {
        black_weight = 2;

        if (black_bishop_count == 2 && white_knight_count == 2)
          black_weight = 4;
      }

      if (white_phase == 3)
        black_weight = 2;
    }

    if (black_phase == 4 && black_queen_count) {
      if (white_phase == 2 && white_knight_count == 2)
        black_weight = 2;

      if (white_phase == 2 && white_knight_count == 1)
        black_weight = 8;

      if (white_phase == 2 && white_knight_count == 0)
        black_weight = 7;

      if (white_phase == 3)
        black_weight = 1;

      if (white_phase == 4)
        black_weight = 1;
    }

    if (black_phase == 4 && black_rook_count == 2) {
      if (white_phase == 2 && white_rook_count == 0)
        black_weight = 7;

      if (white_phase == 3)
        black_weight = 2;

      if (white_phase == 4)
        black_weight = 1;
    }

    if (black_phase == 4 && black_rook_count == 1) {
      if (white_phase == 3 && white_rook_count == 1)
        black_weight = 3;

      if (white_phase == 3 && white_rook_count == 0)
        black_weight = 2;

      if (white_phase == 4)
        black_weight = 2;
    }

    if (black_phase == 4 && black_rook_count == 0 && black_queen_count == 0) {
      if (white_phase == 3 && white_rook_count == 1)
        black_weight = 4;

      if (white_phase == 3 && white_rook_count == 0)
        black_weight = 2;

      if (white_phase == 4 && white_queen_count)
        black_weight = 8;

      if (white_phase == 4 && white_queen_count == 0)
        black_weight = 1;
    }

    if (black_phase == 5 && black_queen_count) {
      if (white_phase == 4)
        black_weight = 2;

      if (white_phase == 5)
        black_weight = 1;

      if (white_phase == 4 && white_rook_count == 2) {
        if (black_knight_count)
          black_weight = 3;

        if (black_bishop_count)
          black_weight = 7;
      }

      if (white_phase == 5)
        black_weight = 1;
    }

    if (black_phase == 5 && black_rook_count == 1) {
      if (white_phase == 4 && white_queen_count)
        black_weight = 9;

      if (white_phase == 4 && white_rook_count == 2)
        black_weight = 7;

      if (white_phase == 4 && white_rook_count == 1)
        black_weight = 3;

      if (white_phase == 4 && white_queen_count == 0 && white_rook_count == 0)
        black_weight = 1;

      if (white_phase == 5)
        black_weight = 2;
    }

    if (black_phase == 5 && black_rook_count == 2) {
      if (white_phase == 4 && white_queen_count && black_bishop_count == 1)
        black_weight = 8;

      if (white_phase == 4 && white_queen_count && black_knight_count == 1)
        black_weight = 7;

      if (white_phase == 4 && white_rook_count == 2)
        black_weight = 3;

      if (white_phase == 4 && white_rook_count == 1)
        black_weight = 2;

      if (white_phase == 4 && white_queen_count == 0 && white_rook_count == 0)
        black_weight = 1;

      if (white_phase == 5)
        black_weight = 1;
    }

    if (black_phase == 6 && black_queen_count && black_rook_count) {
      if (white_phase == 4 && white_queen_count == 0 && white_rook_count == 0)
        black_weight = 2;

      if (white_phase == 5 && white_queen_count)
        black_weight = 1;

      if (white_phase == 4 && white_rook_count == 1)
        black_weight = 6;

      if (white_phase == 4 && white_rook_count == 2)
        black_weight = 3;

      if (white_phase == 5 && white_rook_count)
        black_weight = 1;

      if (white_phase == 6)
        black_weight = 1;
    }

    if (black_phase == 6 && black_queen_count && black_rook_count == 0) {
      if (white_phase == 4 && white_queen_count == 0 && white_rook_count == 0)
        black_weight = 5;

      if (white_phase == 5 && white_queen_count)
        black_weight = 2;

      if (white_phase == 5 && white_rook_count == 2)
        black_weight = 2;

      if (white_phase == 5 && white_rook_count == 1)
        black_weight = 1;

      if (white_phase == 6)
        black_weight = 1;
    }

    if (black_phase == 6 && black_queen_count == 0 && black_rook_count == 2) {
      if (white_phase == 5 && white_queen_count)
        black_weight = 7;

      if (white_phase == 5 && white_rook_count == 1)
        black_weight = 1;

      if (white_phase == 5 && white_rook_count == 2)
        black_weight = 2;

      if (white_phase == 6)
        black_weight = 1;
    }

    if (black_phase == 6 && black_queen_count == 0 && black_rook_count == 1) {
      if (white_phase == 5 && white_queen_count)
        black_weight = 9;

      if (white_phase == 5 && white_rook_count == 2)
        black_weight = 3;

      if (white_phase == 5 && white_rook_count == 1)
        black_weight = 2;

      if (white_phase == 6)
        black_weight = 1;

      if (white_phase == 6 && white_queen_count)
        black_weight = 2;

      if (white_phase == 6 && white_queen_count && white_rook_count)
        black_weight = 4;
    }

    if (black_phase >= 7) {
      if (black_value > white_value + 4)
        black_weight = 9;

      if (black_value == white_value + 4)
        black_weight = 7;

      if (black_value == white_value + 3)
        black_weight = 4;

      if (black_value == white_value + 2)
        black_weight = 2;

      if (black_value < white_value + 2)
        black_weight = 1;
    }
  }

  if (black_pawn_count == 1) {
    if (white_phase == 1) {
      if (black_phase == 1)
        black_weight = 3;

      if (black_phase == 2 && black_knight_count == 2) {
        if (white_pawn_count == 0)
          black_weight = 3;
        else
          black_weight = 5;
      }

      if (black_phase == 2 && black_rook_count == 1)
        black_weight = 7;
    }

    if (white_phase == 2 && white_rook_count == 1 && black_phase == 2 && black_rook_count == 1)
      black_weight = 8;

    if (white_phase == 2 && white_rook_count == 0 && black_phase == 2)
      black_weight = 4;

    if (white_phase >= 3 && white_minor_count > 0 && black_phase == white_phase)
      black_weight = 3;

    if (white_phase >= 3 && white_minor_count == 0 && black_phase == white_phase)
      black_weight = 5;

    if (white_phase == 4 && white_queen_count == 1 && black_phase == white_phase)
      black_weight = 7;
  }
  return black_weight;
}

uint64_t compute_material_value(const int white_pawn_count, const int white_knight_count,
                                const int white_bishop_count, const int white_rook_count,
                                const int white_queen_count, const int black_pawn_count,
                                const int black_knight_count, const int black_bishop_count,
                                const int black_rook_count, const int black_queen_count) {
  uint64_t value = 0;
  value += (white_bishop_count / 2 - black_bishop_count / 2) * mat_bishop_pair_value;
  value += (white_pawn_count - black_pawn_count) * mat_pawn_value;
  value += (white_knight_count - black_knight_count) * mat_knight_value;
  value += (white_rook_count - black_rook_count) * mat_rook_value;
  value += (white_queen_count - black_queen_count) * mat_queen_value;
  value += (white_bishop_count - black_bishop_count) * mat_bishop_value;

  if (white_rook_count == 2)
    value -= mat_2_rook_penalty;

  if (black_rook_count == 2)
    value += mat_2_rook_penalty;

  if (white_queen_count + white_rook_count >= 2)
    value -= mat_queen_rook_penalty;

  if (black_queen_count + black_rook_count >= 2)
    value += mat_queen_rook_penalty;

  value += (white_pawn_count - 5) * white_knight_count * mat_knight_pawn_bonus;
  value -= (black_pawn_count - 5) * black_knight_count * mat_knight_pawn_bonus;

  value -= (white_pawn_count - 5) * white_rook_count * mat_rook_pawn_penalty;
  value += (black_pawn_count - 5) * black_rook_count * mat_rook_pawn_penalty;

  return value;
}

void calculate_material_value(const int c) {
  int
    count_value, weight;
  int n = c;
  const int white_queen_count = n % 2;
  n /= 2;
  const int black_queen_count = n % 2;
  n /= 2;
  const int white_rook_count = n % 3;
  n /= 3;
  const int black_rook_count = n % 3;
  n /= 3;
  const int white_bishop_count_1 = n % 2;
  n /= 2;
  const int white_bishop_count_2 = n % 2;
  n /= 2;
  const int black_bishop_count_1 = n % 2;
  n /= 2;
  const int black_bishop_count_2 = n % 2;
  n /= 2;
  const int white_knight_count = n % 3;
  n /= 3;
  const int black_knight_count = n % 3;
  n /= 3;
  const int white_pawn_count = n % 9;
  n /= 9;
  const int black_pawn_count = n % 9;
  const int white_bishop_count = white_bishop_count_1 + white_bishop_count_2;
  const int black_bishop_count = black_bishop_count_1 + black_bishop_count_2;
  uint64_t value = compute_material_value(white_pawn_count, white_knight_count, white_bishop_count, white_rook_count,
                                          white_queen_count,
                                          black_pawn_count, black_knight_count, black_bishop_count, black_rook_count,
                                          black_queen_count);
  const int phase = 1 * (white_knight_count + white_bishop_count + black_knight_count + black_bishop_count)
    + 3 * (white_rook_count + black_rook_count) + 6 * (white_queen_count + black_queen_count);
  int p1 = value & 0xffff;
  int p2 = (value >> 16 & 0xffff) + (p1 > 0x8000);
  p1 = static_cast<int16_t>(p1);
  int p3 = (value >> 32 & 0xffff) + (p2 > 0x8000);
  p2 = static_cast<int16_t>(p2);
  int p4 = (value >> 48 & 0xffff) + (p3 > 0x8000);
  p3 = static_cast<int16_t>(p3);
  p4 = static_cast<int16_t>(p4);

  if (phase < 8) {
    p4 *= 8 - phase;
    p3 *= phase;
    value = p3 + p4;
    count_value = static_cast<int>(value) / 8;
  }
  else if (phase < 24) {
    p3 *= 24 - phase;
    p2 *= phase - 8;
    value = p2 + p3;
    count_value = static_cast<int>(value) / 16;
  }
  else {
    p2 *= 32 - phase;
    p1 *= phase - 24;
    value = p1 + p2;
    count_value = static_cast<int>(value) / 8;
  }
  const int white_weight = calc_white_weights(white_pawn_count, white_knight_count, white_bishop_count,
                                              white_bishop_count_1,
                                              white_bishop_count_2,
                                              white_rook_count, white_queen_count, black_pawn_count, black_knight_count,
                                              black_bishop_count,
                                              black_bishop_count_1, black_bishop_count_2, black_rook_count,
                                              black_queen_count);
  const int black_weight = calc_black_weights(white_pawn_count, white_knight_count, white_bishop_count,
                                              white_bishop_count_1,
                                              white_bishop_count_2,
                                              white_rook_count, white_queen_count, black_pawn_count, black_knight_count,
                                              black_bishop_count,
                                              black_bishop_count_1, black_bishop_count_2, black_rook_count,
                                              black_queen_count);

  if (count_value > 0)
    weight = white_weight;
  else
    weight = black_weight;
  count_value *= weight;
  count_value /= 10;

  material_table[c].value = count_value;
  material_table[c].token = init_weights(white_pawn_count, white_knight_count,
                                         white_bishop_count, white_bishop_count_1, white_bishop_count_2,
                                         white_rook_count, white_queen_count,
                                         black_pawn_count, black_knight_count, black_bishop_count,
                                         black_bishop_count_1, black_bishop_count_2,
                                         black_rook_count, black_queen_count);
  material_table[c].flag = init_flags(white_pawn_count, white_knight_count, white_bishop_count,
                                      white_bishop_count_1, white_bishop_count_2, white_rook_count, white_queen_count,
                                      black_pawn_count,
                                      black_knight_count, black_bishop_count, black_bishop_count_1,
                                      black_bishop_count_2, black_rook_count,
                                      black_queen_count);
}

void init_material() {
  for (int c = 0; c < 419904; c++)
    calculate_material_value(c);
}

void init_captures() {
  capture_value[white_pawn][black_queen] = (0xd0 << 24) + (0x02 << 20);
  capture_value[white_knight][black_queen] = (0xcf << 24) + (0x02 << 20);
  capture_value[white_king_bishop][black_queen] = (0xce << 24) + (0x02 << 20);
  capture_value[white_queen_bishop][black_queen] = (0xce << 24) + (0x02 << 20);
  capture_value[white_rook][black_queen] = (0xcd << 24) + (0x02 << 20);
  capture_value[white_queen][black_queen] = (0xcc << 24) + (0x01 << 20);

  capture_value[white_pawn][black_rook] = (0xc8 << 24) + (0x02 << 20);
  capture_value[white_knight][black_rook] = (0xc7 << 24) + (0x02 << 20);
  capture_value[white_king_bishop][black_rook] = (0xc6 << 24) + (0x02 << 20);
  capture_value[white_queen_bishop][black_rook] = (0xc6 << 24) + (0x02 << 20);
  capture_value[white_rook][black_rook] = (0xc5 << 24) + (0x01 << 20);
  capture_value[white_queen][black_rook] = (0xc4 << 24) + (0x00 << 20);

  capture_value[white_pawn][black_queen_bishop] = (0xc0 << 24) + (0x02 << 20);
  capture_value[white_knight][black_queen_bishop] = (0xbf << 24) + (0x01 << 20);
  capture_value[white_king_bishop][black_queen_bishop] = (0xbe << 24) + (0x01 << 20);
  capture_value[white_queen_bishop][black_queen_bishop] = (0xbe << 24) + (0x01 << 20);
  capture_value[white_rook][black_queen_bishop] = (0xbd << 24) + (0x00 << 20);
  capture_value[white_queen][black_queen_bishop] = (0xbc << 24) + (0x00 << 20);

  capture_value[white_pawn][black_king_bishop] = (0xc0 << 24) + (0x02 << 20);
  capture_value[white_knight][black_king_bishop] = (0xbf << 24) + (0x01 << 20);
  capture_value[white_king_bishop][black_king_bishop] = (0xbe << 24) + (0x01 << 20);
  capture_value[white_queen_bishop][black_king_bishop] = (0xbe << 24) + (0x01 << 20);
  capture_value[white_rook][black_king_bishop] = (0xbd << 24) + (0x00 << 20);
  capture_value[white_queen][black_king_bishop] = (0xbc << 24) + (0x00 << 20);

  capture_value[white_pawn][black_knight] = (0xb8 << 24) + (0x02 << 20);
  capture_value[white_knight][black_knight] = (0xb7 << 24) + (0x01 << 20);
  capture_value[white_king_bishop][black_knight] = (0xb6 << 24) + (0x01 << 20);
  capture_value[white_queen_bishop][black_knight] = (0xb6 << 24) + (0x01 << 20);
  capture_value[white_rook][black_knight] = (0xb5 << 24) + (0x00 << 20);
  capture_value[white_queen][black_knight] = (0xb4 << 24) + (0x00 << 20);

  capture_value[white_pawn][black_pawn] = (0xb0 << 24) + (0x01 << 20);
  capture_value[white_knight][black_pawn] = (0xaf << 24) + (0x00 << 20);
  capture_value[white_king_bishop][black_pawn] = (0xae << 24) + (0x00 << 20);
  capture_value[white_queen_bishop][black_pawn] = (0xae << 24) + (0x00 << 20);
  capture_value[white_rook][black_pawn] = (0xad << 24) + (0x00 << 20);
  capture_value[white_queen][black_pawn] = (0xac << 24) + (0x00 << 20);

  capture_value[white_king][black_queen] = (0xcb << 24) + (0x00 << 20);
  capture_value[white_king][black_rook] = (0xc3 << 24) + (0x00 << 20);
  capture_value[white_king][black_queen_bishop] = (0xbb << 24) + (0x00 << 20);
  capture_value[white_king][black_king_bishop] = (0xbb << 24) + (0x00 << 20);
  capture_value[white_king][black_knight] = (0xb3 << 24) + (0x00 << 20);
  capture_value[white_king][black_pawn] = (0xab << 24) + (0x00 << 20);

  capture_value[white_king][0] = (0x07 << 24) + (0x00 << 15);
  capture_value[white_pawn][0] = (0x06 << 24) + (0x01 << 15);
  capture_value[white_knight][0] = (0x05 << 24) + (0x01 << 15);
  capture_value[white_king_bishop][0] = (0x04 << 24) + (0x01 << 15);
  capture_value[white_queen_bishop][0] = (0x04 << 24) + (0x01 << 15);
  capture_value[white_rook][0] = (0x03 << 24) + (0x01 << 15);
  capture_value[white_queen][0] = (0x02 << 24) + (0x01 << 15);

  capture_value[black_pawn][white_queen] = (0xd0 << 24) + (0x02 << 20);
  capture_value[black_knight][white_queen] = (0xcf << 24) + (0x02 << 20);
  capture_value[black_queen_bishop][white_queen] = (0xce << 24) + (0x02 << 20);
  capture_value[black_king_bishop][white_queen] = (0xce << 24) + (0x02 << 20);
  capture_value[black_rook][white_queen] = (0xcd << 24) + (0x02 << 20);
  capture_value[black_queen][white_queen] = (0xcc << 24) + (0x01 << 20);

  capture_value[black_pawn][white_rook] = (0xc8 << 24) + (0x02 << 20);
  capture_value[black_knight][white_rook] = (0xc7 << 24) + (0x02 << 20);
  capture_value[black_queen_bishop][white_rook] = (0xc6 << 24) + (0x02 << 20);
  capture_value[black_king_bishop][white_rook] = (0xc6 << 24) + (0x02 << 20);
  capture_value[black_rook][white_rook] = (0xc5 << 24) + (0x01 << 20);
  capture_value[black_queen][white_rook] = (0xc4 << 24) + (0x00 << 20);

  capture_value[black_pawn][white_king_bishop] = (0xc0 << 24) + (0x02 << 20);
  capture_value[black_knight][white_king_bishop] = (0xbf << 24) + (0x01 << 20);
  capture_value[black_queen_bishop][white_king_bishop] = (0xbe << 24) + (0x01 << 20);
  capture_value[black_king_bishop][white_king_bishop] = (0xbe << 24) + (0x01 << 20);
  capture_value[black_rook][white_king_bishop] = (0xbd << 24) + (0x00 << 20);
  capture_value[black_queen][white_king_bishop] = (0xbc << 24) + (0x00 << 20);

  capture_value[black_pawn][white_queen_bishop] = (0xc0 << 24) + (0x02 << 20);
  capture_value[black_knight][white_queen_bishop] = (0xbf << 24) + (0x01 << 20);
  capture_value[black_queen_bishop][white_queen_bishop] = (0xbe << 24) + (0x01 << 20);
  capture_value[black_king_bishop][white_queen_bishop] = (0xbe << 24) + (0x01 << 20);
  capture_value[black_rook][white_queen_bishop] = (0xbd << 24) + (0x00 << 20);
  capture_value[black_queen][white_queen_bishop] = (0xbc << 24) + (0x00 << 20);

  capture_value[black_pawn][white_knight] = (0xb8 << 24) + (0x02 << 20);
  capture_value[black_knight][white_knight] = (0xb7 << 24) + (0x01 << 20);
  capture_value[black_queen_bishop][white_knight] = (0xb6 << 24) + (0x01 << 20);
  capture_value[black_king_bishop][white_knight] = (0xb6 << 24) + (0x01 << 20);
  capture_value[black_rook][white_knight] = (0xb5 << 24) + (0x00 << 20);
  capture_value[black_queen][white_knight] = (0xb4 << 24) + (0x00 << 20);

  capture_value[black_pawn][white_pawn] = (0xb0 << 24) + (0x01 << 20);
  capture_value[black_knight][white_pawn] = (0xaf << 24) + (0x00 << 20);
  capture_value[black_queen_bishop][white_pawn] = (0xae << 24) + (0x00 << 20);
  capture_value[black_king_bishop][white_pawn] = (0xae << 24) + (0x00 << 20);
  capture_value[black_rook][white_pawn] = (0xad << 24) + (0x00 << 20);
  capture_value[black_queen][white_pawn] = (0xac << 24) + (0x00 << 20);

  capture_value[black_king][white_queen] = (0xcb << 24) + (0x00 << 20);
  capture_value[black_king][white_rook] = (0xc3 << 24) + (0x00 << 20);
  capture_value[black_king][white_king_bishop] = (0xbb << 24) + (0x00 << 20);
  capture_value[black_king][white_queen_bishop] = (0xbb << 24) + (0x00 << 20);
  capture_value[black_king][white_knight] = (0xb3 << 24) + (0x00 << 20);
  capture_value[black_king][white_pawn] = (0xab << 24) + (0x00 << 20);

  capture_value[black_king][0] = (0x07 << 24) + (0x00 << 15);
  capture_value[black_pawn][0] = (0x06 << 24) + (0x01 << 15);
  capture_value[black_knight][0] = (0x05 << 24) + (0x01 << 15);
  capture_value[black_queen_bishop][0] = (0x04 << 24) + (0x01 << 15);
  capture_value[black_king_bishop][0] = (0x04 << 24) + (0x01 << 15);
  capture_value[black_rook][0] = (0x03 << 24) + (0x01 << 15);
  capture_value[black_queen][0] = (0x02 << 24) + (0x01 << 15);
}

void make_white_castle(const int to) {
  if (to == g1) {
    board.piece[occupied_white] ^= static_cast<uint64_t>(1) << f1 | static_cast<uint64_t>(1) << h1;
    board.piece[white_rook] ^= static_cast<uint64_t>(1) << f1 | static_cast<uint64_t>(1) << h1;
    board.occupied_total ^= static_cast<uint64_t>(1) << f1 | static_cast<uint64_t>(1) << h1;
    board.occupied_90_left ^= static_cast<uint64_t>(1) << left_90[f1] | static_cast<uint64_t>(1) << left_90[h1];
    board.occupied_45_left ^= static_cast<uint64_t>(1) << left_45[f1] | static_cast<uint64_t>(1) << left_45[h1];
    board.occupied_45_right ^= static_cast<uint64_t>(1) << right_45[f1] | static_cast<uint64_t>(1) << right_45[h1];
    position->pst_value += pst[white_rook][f1] - pst[white_rook][h1];
    position->hash_key ^= rand_hash_table[white_rook][f1] ^ rand_hash_table[white_rook][h1];
    board.square[h1] = 0;
    board.square[f1] = white_rook;
  }
  else if (to == c1) {
    board.piece[occupied_white] ^= static_cast<uint64_t>(1) << a1 | static_cast<uint64_t>(1) << d1;
    board.piece[white_rook] ^= static_cast<uint64_t>(1) << a1 | static_cast<uint64_t>(1) << d1;
    board.occupied_total ^= static_cast<uint64_t>(1) << a1 | static_cast<uint64_t>(1) << d1;
    board.occupied_90_left ^= static_cast<uint64_t>(1) << left_90[a1] | static_cast<uint64_t>(1) << left_90[d1];
    board.occupied_45_left ^= static_cast<uint64_t>(1) << left_45[a1] | static_cast<uint64_t>(1) << left_45[d1];
    board.occupied_45_right ^= static_cast<uint64_t>(1) << right_45[a1] | static_cast<uint64_t>(1) << right_45[d1];
    position->pst_value += pst[white_rook][d1] - pst[white_rook][a1];
    position->hash_key ^= rand_hash_table[white_rook][a1] ^ rand_hash_table[white_rook][d1];
    board.square[a1] = 0;
    board.square[d1] = white_rook;
  }
}

void make_black_castle(const int to) {
  if (to == g8) {
    board.piece[occupied_black] ^= static_cast<uint64_t>(1) << f8 | static_cast<uint64_t>(1) << h8;
    board.piece[black_rook] ^= static_cast<uint64_t>(1) << f8 | static_cast<uint64_t>(1) << h8;
    board.occupied_total ^= static_cast<uint64_t>(1) << f8 | static_cast<uint64_t>(1) << h8;
    board.occupied_90_left ^= static_cast<uint64_t>(1) << left_90[f8] | static_cast<uint64_t>(1) << left_90[h8];
    board.occupied_45_left ^= static_cast<uint64_t>(1) << left_45[f8] | static_cast<uint64_t>(1) << left_45[h8];
    board.occupied_45_right ^= static_cast<uint64_t>(1) << right_45[f8] | static_cast<uint64_t>(1) << right_45[h8];
    position->pst_value += pst[black_rook][f8] - pst[black_rook][h8];
    position->hash_key ^= rand_hash_table[black_rook][f8] ^ rand_hash_table[black_rook][h8];
    board.square[h8] = 0;
    board.square[f8] = black_rook;
  }
  else if (to == c8) {
    board.piece[occupied_black] ^= static_cast<uint64_t>(1) << a8 | static_cast<uint64_t>(1) << d8;
    board.piece[black_rook] ^= static_cast<uint64_t>(1) << a8 | static_cast<uint64_t>(1) << d8;
    board.occupied_total ^= static_cast<uint64_t>(1) << a8 | static_cast<uint64_t>(1) << d8;
    board.occupied_90_left ^= static_cast<uint64_t>(1) << left_90[a8] | static_cast<uint64_t>(1) << left_90[d8];
    board.occupied_45_left ^= static_cast<uint64_t>(1) << left_45[a8] | static_cast<uint64_t>(1) << left_45[d8];
    board.occupied_45_right ^= static_cast<uint64_t>(1) << right_45[a8] | static_cast<uint64_t>(1) << right_45[d8];
    position->pst_value += pst[black_rook][d8] - pst[black_rook][a8];
    position->hash_key ^= rand_hash_table[black_rook][a8] ^ rand_hash_table[black_rook][d8];
    board.square[a8] = 0;
    board.square[d8] = black_rook;
  }
}

void undo_white_castle(const int to) {
  if (to == g1) {
    board.piece[occupied_white] ^= static_cast<uint64_t>(1) << f1 | static_cast<uint64_t>(1) << h1;
    board.piece[white_rook] ^= static_cast<uint64_t>(1) << f1 | static_cast<uint64_t>(1) << h1;
    board.square[f1] = 0;
    board.square[h1] = white_rook;
    board.occupied_total ^= static_cast<uint64_t>(1) << f1 | static_cast<uint64_t>(1) << h1;
    board.occupied_90_left ^= static_cast<uint64_t>(1) << left_90[f1] | static_cast<uint64_t>(1) << left_90[h1];
    board.occupied_45_left ^= static_cast<uint64_t>(1) << left_45[f1] | static_cast<uint64_t>(1) << left_45[h1];
    board.occupied_45_right ^= static_cast<uint64_t>(1) << right_45[f1] | static_cast<uint64_t>(1) << right_45[h1];
  }
  else if (to == c1) {
    board.piece[occupied_white] ^= static_cast<uint64_t>(1) << a1 | static_cast<uint64_t>(1) << d1;
    board.piece[white_rook] ^= static_cast<uint64_t>(1) << a1 | static_cast<uint64_t>(1) << d1;
    board.square[d1] = 0;
    board.square[a1] = white_rook;
    board.occupied_total ^= static_cast<uint64_t>(1) << a1 | static_cast<uint64_t>(1) << d1;
    board.occupied_90_left ^= static_cast<uint64_t>(1) << left_90[a1] | static_cast<uint64_t>(1) << left_90[d1];
    board.occupied_45_left ^= static_cast<uint64_t>(1) << left_45[a1] | static_cast<uint64_t>(1) << left_45[d1];
    board.occupied_45_right ^= static_cast<uint64_t>(1) << right_45[a1] | static_cast<uint64_t>(1) << right_45[d1];
  }
}

void undo_black_castle(const int to) {
  if (to == g8) {
    board.piece[occupied_black] ^= static_cast<uint64_t>(1) << f8 | static_cast<uint64_t>(1) << h8;
    board.piece[black_rook] ^= static_cast<uint64_t>(1) << f8 | static_cast<uint64_t>(1) << h8;
    board.square[f8] = 0;
    board.square[h8] = black_rook;
    board.occupied_total ^= static_cast<uint64_t>(1) << f8 | static_cast<uint64_t>(1) << h8;
    board.occupied_90_left ^= static_cast<uint64_t>(1) << left_90[f8] | static_cast<uint64_t>(1) << left_90[h8];
    board.occupied_45_left ^= static_cast<uint64_t>(1) << left_45[f8] | static_cast<uint64_t>(1) << left_45[h8];
    board.occupied_45_right ^= static_cast<uint64_t>(1) << right_45[f8] | static_cast<uint64_t>(1) << right_45[h8];
  }
  else if (to == c8) {
    board.piece[occupied_black] ^= static_cast<uint64_t>(1) << a8 | static_cast<uint64_t>(1) << d8;
    board.piece[black_rook] ^= static_cast<uint64_t>(1) << a8 | static_cast<uint64_t>(1) << d8;
    board.square[d8] = 0;
    board.square[a8] = black_rook;
    board.occupied_total ^= static_cast<uint64_t>(1) << a8 | static_cast<uint64_t>(1) << d8;
    board.occupied_90_left ^= static_cast<uint64_t>(1) << left_90[a8] | static_cast<uint64_t>(1) << left_90[d8];
    board.occupied_45_left ^= static_cast<uint64_t>(1) << left_45[a8] | static_cast<uint64_t>(1) << left_45[d8];
    board.occupied_45_right ^= static_cast<uint64_t>(1) << right_45[a8] | static_cast<uint64_t>(1) << right_45[d8];
  }
}

void white_make(const uint32_t move) {
  nodes_white++;

  if ((nodes_white & 4095) == 0)
    check_if_done(0);
  memcpy(position + 1, position, 32);
  const int from = move >> 6 & 077;
  const int to = move & 077;
  int piece = board.square[from];
  position++;
  position->reversible++;
  position->move = move;
  const int fl = castle_table[from] & castle_table[to] & position->castle;
  position->hash_key ^= rand_hash_castle[position->castle ^ fl];
  position->pawn_hash_key ^= rand_hash_castle[position->castle ^ fl];
  position->castle = fl;

  if (position->en_passant) {
    position->hash_key ^= rand_hash_en_passant[position->en_passant & 7];
    position->en_passant = 0;
  }
  board.square[from] = 0;
  uint64_t mask = square_clear[from];
  board.piece[occupied_white] &= mask;
  board.piece[piece] &= mask;
  board.occupied_total &= mask;
  board.occupied_90_left &= left_90_clear[from];
  board.occupied_45_left &= left_45_clear[from];
  board.occupied_45_right &= right_45_clear[from];
  position->pst_value += pst[piece][to] - pst[piece][from];
  mask = rand_hash_table[piece][from] ^ rand_hash_table[piece][to];
  const int capture = board.square[to];
  position->capture = capture;
  position->hash_key ^= mask;

  if (piece == white_pawn)
    position->pawn_hash_key ^= mask;
  board.white_to_move ^= 1;
  position->hash_key ^= rand_hash_white_to_move;

  if (piece == white_king) {
    position->pawn_hash_key ^= mask;
    board.white_king = to;
  }

  if (capture) {
    mask = square_clear[to];
    board.piece[occupied_black] &= mask;
    board.piece[capture] &= mask;
    position->material -= material_values[capture];
    position->pst_value -= pst[capture][to];

    if (capture == black_pawn)
      position->pawn_hash_key ^= rand_hash_table[capture][to];
    position->hash_key ^= rand_hash_table[capture][to];
    position->reversible = 0;
  }
  else {
    mask = square_set[to];
    board.occupied_total |= mask;
    board.occupied_90_left |= left_90_set[to];
    board.occupied_45_left |= left_45_set[to];
    board.occupied_45_right |= right_45_set[to];

    if ((move & 070000) == 010000) {
      position->reversible = 0;
      make_white_castle(to);
    }
  }
  board.square[to] = piece;
  board.piece[occupied_white] |= square_set[to];
  board.piece[piece] |= square_set[to];

  if (piece == white_pawn) {
    int z;
    position->reversible = 0;

    if ((move & 070000) == 030000) {
      z = to ^ 8;
      mask = square_clear[z];
      board.piece[occupied_black] &= mask;
      board.piece[black_pawn] &= mask;
      board.occupied_total &= mask;
      board.occupied_90_left &= left_90_clear[z];
      board.occupied_45_left &= left_45_clear[z];
      board.occupied_45_right &= right_45_clear[z];
      position->material -= material_values[black_pawn];
      position->pst_value -= pst[black_pawn][z];
      position->hash_key ^= rand_hash_table[black_pawn][z];
      position->pawn_hash_key ^= rand_hash_table[black_pawn][z];
      board.square[z] = 0;
    }
    else if ((move & 070000) >= 040000) {
      piece = promotions_white[(move & 070000) >> 12];

      if (piece == white_king_bishop && square_set[to] & 0xaa55aa55aa55aa55)
        piece = white_queen_bishop;
      board.square[to] = piece;

      if (board.piece[piece])
        position->material |= 0x80000000;
      board.piece[white_pawn] &= square_clear[to];
      board.piece[piece] |= square_set[to];
      position->material +=
        material_values[piece] - material_values[white_pawn];
      position->pst_value += pst[piece][to] - pst[white_pawn][to];
      position->hash_key ^= rand_hash_table[piece][to] ^ rand_hash_table[white_pawn][to];
      position->pawn_hash_key ^= rand_hash_table[white_pawn][to];
    }
    else if ((to ^ from) == 16) {
      if (white_en_passant_table[to & 7] & board.piece[black_pawn]) {
        z = (from + to) >> 1;
        position->en_passant = z;
        position->hash_key ^= rand_hash_en_passant[z & 7];
      }
    }
  }
  stack[++stack_height] = position->hash_key;
}

void white_undo(const uint32_t move) {
  const int from = move >> 6 & 077;
  const int to = move & 077;
  int piece = board.square[to];
  board.white_to_move ^= 1;

  if ((move & 070000) >= 040000) {
    board.piece[piece] &= square_clear[to];
    piece = white_pawn;
  }
  board.square[from] = piece;
  board.square[to] = position->capture;

  if (piece == white_king)
    board.white_king = from;
  uint64_t mask = square_set[from];
  board.piece[occupied_white] |= mask;
  board.piece[piece] |= mask;
  board.occupied_total |= mask;
  board.occupied_90_left |= left_90_set[from];
  board.occupied_45_left |= left_45_set[from];
  board.occupied_45_right |= right_45_set[from];
  mask = square_clear[to];
  board.piece[occupied_white] &= mask;
  board.piece[piece] &= mask;

  if (const int capture = position->capture) {
    mask = ~mask;
    board.piece[occupied_black] |= mask;
    board.piece[capture] |= mask;
  }
  else {
    board.occupied_total &= mask;
    board.occupied_90_left &= left_90_clear[to];
    board.occupied_45_left &= left_45_clear[to];
    board.occupied_45_right &= right_45_clear[to];

    if ((move & 070000) == 010000)
      undo_white_castle(to);
    else if ((move & 070000) == 030000) {
      const int z = to ^ 8;
      board.square[z] = black_pawn;
      mask = square_set[z];
      board.piece[occupied_black] |= mask;
      board.piece[black_pawn] |= mask;
      board.occupied_total |= mask;
      board.occupied_90_left |= left_90_set[z];
      board.occupied_45_left |= left_45_set[z];
      board.occupied_45_right |= right_45_set[z];
    }
  }
  position--;
  stack_height--;
}

void black_make(const uint32_t move) {
  nodes_black++;
  memcpy(position + 1, position, 32);
  const int from = move >> 6 & 077;
  const int to = move & 077;
  int piece = board.square[from];
  position++;
  position->reversible++;
  position->move = move;
  const int fl = castle_table[from] & castle_table[to] & position->castle;
  position->hash_key ^= rand_hash_castle[position->castle ^ fl];
  position->pawn_hash_key ^= rand_hash_castle[position->castle ^ fl];
  position->castle = fl;

  if (position->en_passant) {
    position->hash_key ^= rand_hash_en_passant[position->en_passant & 7];
    position->en_passant = 0;
  }
  board.square[from] = 0;
  uint64_t mask = square_clear[from];
  board.piece[occupied_black] &= mask;
  board.piece[piece] &= mask;
  board.occupied_total &= mask;
  board.occupied_90_left &= left_90_clear[from];
  board.occupied_45_left &= left_45_clear[from];
  board.occupied_45_right &= right_45_clear[from];
  position->pst_value += pst[piece][to] - pst[piece][from];
  mask = rand_hash_table[piece][from] ^ rand_hash_table[piece][to];
  const int capture = board.square[to];
  position->capture = capture;
  position->hash_key ^= mask;

  if (piece == black_pawn)
    position->pawn_hash_key ^= mask;
  board.white_to_move ^= 1;
  position->hash_key ^= rand_hash_white_to_move;

  if (piece == black_king) {
    position->pawn_hash_key ^= mask;
    board.black_king = to;
  }

  if (capture) {
    mask = square_clear[to];
    board.piece[occupied_white] &= mask;
    board.piece[capture] &= mask;
    position->material -= material_values[capture];
    position->pst_value -= pst[capture][to];

    if (capture == white_pawn)
      position->pawn_hash_key ^= rand_hash_table[capture][to];
    position->hash_key ^= rand_hash_table[capture][to];
    position->reversible = 0;
  }
  else {
    mask = square_set[to];
    board.occupied_total |= mask;
    board.occupied_90_left |= left_90_set[to];
    board.occupied_45_left |= left_45_set[to];
    board.occupied_45_right |= right_45_set[to];

    if ((move & 070000) == 010000) {
      position->reversible = 0;
      make_black_castle(to);
    }
  }
  board.square[to] = piece;
  board.piece[occupied_black] |= square_set[to];
  board.piece[piece] |= square_set[to];

  if (piece == black_pawn) {
    int z;
    position->reversible = 0;

    if ((move & 070000) == 030000) {
      z = to ^ 8;
      mask = square_clear[z];
      board.piece[occupied_white] &= mask;
      board.piece[white_pawn] &= mask;
      board.occupied_total &= mask;
      board.occupied_90_left &= left_90_clear[z];
      board.occupied_45_left &= left_45_clear[z];
      board.occupied_45_right &= right_45_clear[z];
      position->material -= material_values[white_pawn];
      position->pst_value -= pst[white_pawn][z];
      position->hash_key ^= rand_hash_table[white_pawn][z];
      position->pawn_hash_key ^= rand_hash_table[white_pawn][z];
      board.square[z] = 0;
    }
    else if ((move & 070000) >= 040000) {
      piece = promotions_black[(move & 070000) >> 12];

      if (piece == black_queen_bishop && square_set[to] & 0xaa55aa55aa55aa55)
        piece = black_king_bishop;
      board.square[to] = piece;

      if (board.piece[piece])
        position->material |= 0x80000000;
      board.piece[black_pawn] &= square_clear[to];
      board.piece[piece] |= square_set[to];
      position->material +=
        material_values[piece] - material_values[black_pawn];
      position->pst_value += pst[piece][to] - pst[black_pawn][to];
      position->hash_key ^= rand_hash_table[piece][to] ^ rand_hash_table[black_pawn][to];
      position->pawn_hash_key ^= rand_hash_table[black_pawn][to];
    }
    else if ((to ^ from) == 16) {
      if (black_en_passant_table[to & 7] & board.piece[white_pawn]) {
        z = (from + to) >> 1;
        position->en_passant = z;
        position->hash_key ^= rand_hash_en_passant[z & 7];
      }
    }
  }
  stack[++stack_height] = position->hash_key;
}

void black_undo(const uint32_t move) {
  const int from = move >> 6 & 077;
  const int to = move & 077;
  int piece = board.square[to];
  board.white_to_move ^= 1;

  if ((move & 070000) >= 040000) {
    board.piece[piece] &= square_clear[to];
    piece = black_pawn;
  }
  board.square[from] = piece;
  board.square[to] = position->capture;

  if (piece == black_king)
    board.black_king = from;
  uint64_t mask = square_set[from];
  board.piece[occupied_black] |= mask;
  board.piece[piece] |= mask;
  board.occupied_total |= mask;
  board.occupied_90_left |= left_90_set[from];
  board.occupied_45_left |= left_45_set[from];
  board.occupied_45_right |= right_45_set[from];
  mask = square_clear[to];
  board.piece[occupied_black] &= mask;
  board.piece[piece] &= mask;

  if (const int capture = position->capture) {
    mask = ~mask;
    board.piece[occupied_white] |= mask;
    board.piece[capture] |= mask;
  }
  else {
    board.occupied_total &= mask;
    board.occupied_90_left &= left_90_clear[to];
    board.occupied_45_left &= left_45_clear[to];
    board.occupied_45_right &= right_45_clear[to];

    if ((move & 070000) == 010000)
      undo_black_castle(to);
    else if ((move & 070000) == 030000) {
      const int z = to ^ 8;
      board.square[z] = white_pawn;
      mask = square_set[z];
      board.piece[occupied_white] |= mask;
      board.piece[white_pawn] |= mask;
      board.occupied_total |= mask;
      board.occupied_90_left |= left_90_set[z];
      board.occupied_45_left |= left_45_set[z];
      board.occupied_45_right |= right_45_set[z];
    }
  }
  position--;
  stack_height--;
}

void make(const uint32_t move) {
  if (board.white_to_move) {
    if (nodes_white & 4095)
      nodes_white--;
    white_make(move);
  }
  else {
    nodes_black--;
    black_make(move);
  }
}

void undo(const uint32_t move) {
  if (!board.white_to_move)
    white_undo(move);
  else
    black_undo(move);
}

void eval_mobility() {
  uint64_t U, T, diagonal_attack, orthogonal_attack;
  int b;
  position->white_xray = 0;
  position->black_xray = 0;
  uint64_t A = attack_king[board.white_king];
  position->white_attack = A;

  if (A & board.piece[black_king])
    position->black_king_check |= square_set[board.white_king];
  else
    position->black_king_check = 0;
  A = attack_king[board.black_king];
  position->black_attack = A;

  if (A & board.piece[white_king])
    position->white_king_check |= square_set[board.black_king];
  else
    position->white_king_check = 0;

  for (U = board.piece[white_knight]; U; U &= U - 1) {
    b = bsf(U);
    A = attack_knight[b];
    position->white_attack |= A;

    if (A & board.piece[black_king])
      position->black_king_check |= square_set[b];
  }

  for (U = board.piece[white_king_bishop] | board.piece[white_queen_bishop]; U; U &= U - 1) {
    b = bsf(U);
    A = bitboard_line_obscured[1][b][board.occupied_45_right >> line_turn[1][b] & 077]
      | bitboard_line_obscured[0][b][board.occupied_45_left >> line_turn[0][b] & 077];
    position->white_attack |= A;

    if (A & board.piece[black_king])
      position->black_king_check |= square_set[b];
    else if (board.piece[black_king] & diagonal[b]) {
      T = (bitboard_line_obscured[1][board.black_king][board.occupied_45_right
          >> line_turn[1][board.black_king] & 077]
        | bitboard_line_obscured[0][board.black_king][board.occupied_45_left
          >> line_turn[0][board.black_king] & 077]) & A;
      position->white_xray |= T;

      if (T)
        xray_table[bsf(T)] = b;
    }
  }

  for (U = board.piece[white_rook]; U; U &= U - 1) {
    b = bsf(U);
    A = bitboard_line_obscured[2][b][board.occupied_total >> line_turn[2][b] & 077]
      | bitboard_line_obscured[3][b][board.occupied_90_left >> line_turn[3][b] & 077];
    position->white_attack |= A;

    if (A & board.piece[black_king])
      position->black_king_check |= square_set[b];
    else if (board.piece[black_king] & orthogonal[b]) {
      T = (bitboard_line_obscured[2][board.black_king][board.occupied_total
          >> line_turn[2][board.black_king] & 077]
        | bitboard_line_obscured[3][board.black_king][board.occupied_90_left
          >> line_turn[3][board.black_king] & 077]) & A;
      position->white_xray |= T;

      if (T)
        xray_table[bsf(T)] = b;
    }
  }

  for (U = board.piece[white_queen]; U; U &= U - 1) {
    b = bsf(U);
    orthogonal_attack =
      bitboard_line_obscured[2][b][board.occupied_total >> line_turn[2][b] & 077]
      | bitboard_line_obscured[3][b][board.occupied_90_left >> line_turn[3][b] & 077];
    diagonal_attack =
      bitboard_line_obscured[1][b][board.occupied_45_right >> line_turn[1][b] & 077]
      | bitboard_line_obscured[0][b][board.occupied_45_left >> line_turn[0][b] & 077];
    A = diagonal_attack | orthogonal_attack;
    position->white_attack |= A;

    if (A & board.piece[black_king])
      position->black_king_check |= square_set[b];
    else if (board.piece[black_king] & diagonal[b]) {
      T = (bitboard_line_obscured[1][board.black_king][board.occupied_45_right
          >> line_turn[1][board.black_king] & 077]
        | bitboard_line_obscured[0][board.black_king][board.occupied_45_left
          >> line_turn[0][board.black_king] & 077]) & diagonal_attack;
      position->white_xray |= T;

      if (T)
        xray_table[bsf(T)] = b;
    }
    else if (board.piece[black_king] & orthogonal[b]) {
      T = (bitboard_line_obscured[2][board.black_king][board.occupied_total
          >> line_turn[2][board.black_king] & 077]
        | bitboard_line_obscured[3][board.black_king][board.occupied_90_left
          >> line_turn[3][board.black_king] & 077]) & orthogonal_attack;
      position->white_xray |= T;

      if (T)
        xray_table[bsf(T)] = b;
    }
  }

  for (U = board.piece[black_knight]; U; U &= U - 1) {
    b = bsf(U);
    A = attack_knight[b];
    position->black_attack |= A;

    if (A & board.piece[white_king])
      position->white_king_check |= square_set[b];
  }

  for (U = board.piece[black_queen_bishop] | board.piece[black_king_bishop]; U; U &= U - 1) {
    b = bsf(U);
    A = bitboard_line_obscured[1][b][board.occupied_45_right >> line_turn[1][b] & 077]
      | bitboard_line_obscured[0][b][board.occupied_45_left >> line_turn[0][b] & 077];
    position->black_attack |= A;

    if (A & board.piece[white_king])
      position->white_king_check |= square_set[b];
    else if (board.piece[white_king] & diagonal[b]) {
      T = (bitboard_line_obscured[1][board.white_king][board.occupied_45_right
          >> line_turn[1][board.white_king] & 077]
        | bitboard_line_obscured[0][board.white_king][board.occupied_45_left
          >> line_turn[0][board.white_king] & 077]) & A;
      position->black_xray |= T;

      if (T)
        xray_table[bsf(T)] = b;
    }
  }

  for (U = board.piece[black_rook]; U; U &= U - 1) {
    b = bsf(U);
    A = bitboard_line_obscured[2][b][board.occupied_total >> line_turn[2][b] & 077]
      | bitboard_line_obscured[3][b][board.occupied_90_left >> line_turn[3][b] & 077];
    position->black_attack |= A;

    if (A & board.piece[white_king])
      position->white_king_check |= square_set[b];
    else if (board.piece[white_king] & orthogonal[b]) {
      T = (bitboard_line_obscured[2][board.white_king][board.occupied_total
          >> line_turn[2][board.white_king] & 077]
        | bitboard_line_obscured[3][board.white_king][board.occupied_90_left
          >> line_turn[3][board.white_king] & 077]) & A;
      position->black_xray |= T;

      if (T)
        xray_table[bsf(T)] = b;
    }
  }

  for (U = board.piece[black_queen]; U; U &= U - 1) {
    b = bsf(U);
    diagonal_attack =
      bitboard_line_obscured[1][b][board.occupied_45_right >> line_turn[1][b] & 077]
      | bitboard_line_obscured[0][b][board.occupied_45_left >> line_turn[0][b] & 077];
    orthogonal_attack =
      bitboard_line_obscured[2][b][board.occupied_total >> line_turn[2][b] & 077]
      | bitboard_line_obscured[3][b][board.occupied_90_left >> line_turn[3][b] & 077];
    A = diagonal_attack | orthogonal_attack;
    position->black_attack |= A;

    if (A & board.piece[white_king])
      position->white_king_check |= square_set[b];
    else if (board.piece[white_king] & diagonal[b]) {
      T = (bitboard_line_obscured[1][board.white_king][board.occupied_45_right
          >> line_turn[1][board.white_king] & 077]
        | bitboard_line_obscured[0][board.white_king][board.occupied_45_left
          >> line_turn[0][board.white_king] & 077]) & diagonal_attack;
      position->black_xray |= T;

      if (T)
        xray_table[bsf(T)] = b;
    }
    else if (board.piece[white_king] & orthogonal[b]) {
      T = (bitboard_line_obscured[2][board.white_king][board.occupied_total
          >> line_turn[2][board.white_king] & 077]
        | bitboard_line_obscured[3][board.white_king][board.occupied_90_left
          >> line_turn[3][board.white_king] & 077]) & orthogonal_attack;
      position->black_xray |= T;

      if (T)
        xray_table[bsf(T)] = b;
    }
  }
  A = (board.piece[white_pawn] & ~0x0101010101010101) << 7;
  T = A & board.piece[black_king];
  position->black_king_check |= T >> 7;
  position->white_attack |= A;
  A = (board.piece[white_pawn] & ~0x8080808080808080) << 9;
  T = A & board.piece[black_king];
  position->black_king_check |= T >> 9;
  position->white_attack |= A;
  A = (board.piece[black_pawn] & ~0x8080808080808080) >> 7;
  T = A & board.piece[white_king];
  position->white_king_check |= T << 7;
  position->black_attack |= A;
  A = (board.piece[black_pawn] & ~0x0101010101010101) >> 9;
  T = A & board.piece[white_king];
  position->white_king_check |= T << 9;
  position->black_attack |= A;
}

void update_white_gain(const int move) {
  if (position->capture && board.square[(move & 077)] != black_pawn)
    return;

  const int sh = board.square[(move & 077)];
  const int d = move & 07777;

  if (const int value = (position - 1)->positional_value - position->positional_value; max_increase[sh][d] < value)
    max_increase[sh][d] = value;

  else if (max_increase[sh][d] > value)
    max_increase[sh][d]--;
}

void update_black_gain(const int move) {
  if (position->capture && board.square[(move & 077)] != white_pawn)
    return;

  const int sh = board.square[(move & 077)];
  const int d = move & 07777;

  if (const int value = position->positional_value - (position - 1)->positional_value; max_increase[sh][d] < value)
    max_increase[sh][d] = value;

  else if (max_increase[sh][d] > value)
    max_increase[sh][d]--;
}

void clear_eval_hash() {
  for (unsigned long long& c : eval_hash)
    c = 0;
}

int eval_material() {
  int value = 975 * (popcnt(board.piece[white_queen]) - popcnt(board.piece[black_queen]));
  value += 500 * (popcnt(board.piece[white_rook]) - popcnt(board.piece[black_rook]));
  value += 325 * (popcnt(board.piece[white_king_bishop] | board.piece[white_queen_bishop])
    - popcnt(board.piece[black_queen_bishop] | board.piece[black_king_bishop]));
  value += 325 * (popcnt(board.piece[white_knight]) - popcnt(board.piece[black_knight]));
  value += 100 * (popcnt(board.piece[white_pawn]) - popcnt(board.piece[black_pawn]));

  if (board.piece[white_king_bishop] && board.piece[white_queen_bishop])
    value += 50;

  if (board.piece[black_queen_bishop] && board.piece[black_king_bishop])
    value -= 50;
  return value;
}

void endgame_pawn_white(const int material_value, const uint8_t token, type_pawn_hash* pawn_value) {
  int square, rank = 0;

  if (pawn_value->pawn_hash_key != position->pawn_hash_key)
    eval_pawns(pawn_value);
  position->white_xray = position->black_xray = 0;
  int score = position->pst_value + pawn_value->score;
  score = static_cast<int16_t>(score & 0xffff);
  int white_leader = 0;
  uint8_t file = pawn_value->white_passed_pawn_file;

  while (file) {
    square = bsr(file_table[bsf(file)] & board.piece[white_pawn]);
    rank = square >> 3;
    file &= file - 1;

    if ((shepherd_white[square] & board.piece[white_king]) == 0) {
      if (board.piece[occupied_white] & open_file_white[square]
        || (board.piece[black_king] & quadrant_black_wtm[square]) == 0)
        continue;

      if (white_leader <= rank)
        white_leader = rank;
    }

    else if (white_leader <= rank)
      white_leader = rank;
  }
  int black_leader = 0;
  file = pawn_value->black_passed_pawn_file;

  while (file) {
    square = bsf(file_table[bsf(file)] & board.piece[black_pawn]);
    rank = rank_8 - (square >> 3);
    file &= file - 1;

    if ((shepherd_black[square] & board.piece[black_king]) == 0) {
      if (board.piece[occupied_black] & open_file_black[square]
        || (board.piece[white_king] & quadrant_white_wtm[square]) == 0)
        continue;

      if (black_leader <= rank)
        black_leader = rank;
    }

    else if (black_leader <= rank)
      black_leader = rank;
  }

  position->score = token * (score + material_value) / 128;
  position->black_king_check = position->white_king_check = 0;

  if (white_leader > black_leader
    && (board.piece[black_pawn] & in_front_black[rank_8 - white_leader + 1]) == 0)
    position->score += 150 + 50 * white_leader;

  if (black_leader > white_leader
    + 1 && (board.piece[white_pawn] & in_front_white[black_leader - 2]) == 0)
    position->score -= 150 + 50 * black_leader;

  uint64_t A = (board.piece[white_pawn] & ~0x0101010101010101) << 7;
  uint64_t T = A & board.piece[black_king];
  position->black_king_check |= T >> 7;
  position->white_attack = A;
  A = (board.piece[white_pawn] & ~0x8080808080808080) << 9;
  T = A & board.piece[black_king];
  position->black_king_check |= T >> 9;
  position->white_attack |= A | attack_king[board.white_king];
  A = (board.piece[black_pawn] & ~0x8080808080808080) >> 7;
  T = A & board.piece[white_king];
  position->white_king_check |= T << 7;
  position->black_attack = A;
  A = (board.piece[black_pawn] & ~0x0101010101010101) >> 9;
  T = A & board.piece[white_king];
  position->white_king_check |= T << 9;
  position->black_attack |= A | attack_king[board.black_king];

  if (board.piece[black_king] & attack_king[board.white_king]) {
    position->black_king_check |= square_set[board.white_king];
    position->white_king_check |= square_set[board.black_king];
  }

  if (position->score > 0 && !board.piece[white_pawn])
    position->score = 0;

  if (position->score < 0 && !board.piece[black_pawn])
    position->score = 0;

  if (position->score > 0) {
    if ((board.piece[white_pawn] & ~0x8080808080808080) == 0
      && (board.piece[black_king]
        | attack_king[board.black_king]) & square_set[h8])
      position->score = 0;

    if ((board.piece[white_pawn] & ~0x0101010101010101) == 0
      && (board.piece[black_king]
        | attack_king[board.black_king]) & square_set[a8])
      position->score = 0;

    if ((position->flag & 28) == 28) {
      if (!score)
        position->score = 0;
      else
        position->score = static_cast<int16_t>(position->pst_value & 0xffff) + 75 * rank + 250;
    }
  }

  if (position->score < 0) {
    if ((board.piece[black_pawn] & ~0x8080808080808080) == 0
      && (board.piece[white_king]
        | attack_king[board.white_king]) & square_set[h1])
      position->score = 0;

    if ((board.piece[black_pawn] & ~0x0101010101010101) == 0
      && (board.piece[white_king]
        | attack_king[board.white_king]) & square_set[a1])
      position->score = 0;

    if ((position->flag & 28) == 28) {
      if (!score)
        position->score = 0;
      else
        position->score = static_cast<int16_t>(position->pst_value & 0xffff) - 75 * rank - 250;
    }
  }
}

void endgame_pawn_black(const int material_value, const uint8_t token, type_pawn_hash* pawn_value) {
  int square, rank = 0;

  if (pawn_value->pawn_hash_key != position->pawn_hash_key)
    eval_pawns(pawn_value);
  position->white_xray = position->black_xray = 0;
  int score = position->pst_value + pawn_value->score;
  score = static_cast<int16_t>(score & 0xffff);
  int white_leader = 0;
  uint8_t file = pawn_value->white_passed_pawn_file;

  while (file) {
    square = bsr(file_table[bsf(file)] & board.piece[white_pawn]);
    rank = square >> 3;
    file &= file - 1;

    if ((shepherd_white[square] & board.piece[white_king]) == 0) {
      if (board.piece[occupied_white] & open_file_white[square]
        || (board.piece[black_king] & quadrant_black_btm[square]) == 0)
        continue;

      if (white_leader <= rank)
        white_leader = rank;
    }

    else if (white_leader <= rank)
      white_leader = rank;
  }
  int black_leader = 0;
  file = pawn_value->black_passed_pawn_file;

  while (file) {
    square = bsf(file_table[bsf(file)] & board.piece[black_pawn]);
    rank = rank_8 - (square >> 3);
    file &= file - 1;

    if ((shepherd_black[square] & board.piece[black_king]) == 0) {
      if (board.piece[occupied_black] & open_file_black[square]
        || (board.piece[white_king] & quadrant_white_btm[square]) == 0)
        continue;

      if (black_leader <= rank)
        black_leader = rank;
    }

    else if (black_leader <= rank)
      black_leader = rank;
  }
  position->score = -(token * (score + material_value)) / 128;
  position->black_king_check = position->white_king_check = 0;

  if (white_leader > black_leader + 1
    && (board.piece[black_pawn] & in_front_black[rank_8 - white_leader + 2]) == 0)
    position->score -= 150 + 50 * white_leader;

  if (black_leader > white_leader
    && (board.piece[white_pawn] & in_front_white[black_leader - 1]) == 0)
    position->score += 150 + 50 * black_leader;
  uint64_t A = (board.piece[white_pawn] & ~0x0101010101010101) << 7;
  uint64_t T = A & board.piece[black_king];
  position->black_king_check |= T >> 7;
  position->white_attack = A;
  A = (board.piece[white_pawn] & ~0x8080808080808080) << 9;
  T = A & board.piece[black_king];
  position->black_king_check |= T >> 9;
  position->white_attack |= A | attack_king[board.white_king];
  A = (board.piece[black_pawn] & ~0x8080808080808080) >> 7;
  T = A & board.piece[white_king];
  position->white_king_check |= T << 7;
  position->black_attack = A;
  A = (board.piece[black_pawn] & ~0x0101010101010101) >> 9;
  T = A & board.piece[white_king];
  position->white_king_check |= T << 9;
  position->black_attack |= A | attack_king[board.black_king];

  if (board.piece[black_king] & attack_king[board.white_king]) {
    position->black_king_check |= square_set[board.white_king];
    position->white_king_check |= square_set[board.black_king];
  }

  if (position->score < 0 && !board.piece[white_pawn])
    position->score = 0;

  if (position->score > 0 && !board.piece[black_pawn])
    position->score = 0;

  if (position->score < 0) {
    if ((board.piece[white_pawn] & ~0x8080808080808080) == 0
      && (board.piece[black_king]
        | attack_king[board.black_king]) & square_set[h8])
      position->score = 0;

    if ((board.piece[white_pawn] & ~0x0101010101010101) == 0
      && (board.piece[black_king]
        | attack_king[board.black_king]) & square_set[a8])
      position->score = 0;

    if ((position->flag & 28) == 28) {
      if (!score)
        position->score = 0;
      else
        position->score =
          -static_cast<int16_t>(position->pst_value & 0xffff) - 75 * rank - 250;
    }
  }

  if (position->score > 0) {
    if ((board.piece[black_pawn] & ~0x8080808080808080) == 0
      && (board.piece[white_king]
        | attack_king[board.white_king]) & square_set[h1])
      position->score = 0;

    if ((board.piece[black_pawn] & ~0x0101010101010101) == 0
      && (board.piece[white_king]
        | attack_king[board.white_king]) & square_set[a1])
      position->score = 0;

    if ((position->flag & 28) == 28) {
      if (!score)
        position->score = 0;
      else
        position->score =
          -static_cast<int16_t>(position->pst_value & 0xffff) + 75 * rank + 250;
    }
  }
}

void eval(int min, int max, int move) {
  type_pawn_hash* pawn_value;
  int index, material_value, score;
  int b, rank, anti_phase, phase;
  int to, capture, white_king_square, black_king_square;
  uint64_t U, white_king_attack, black_king_attack, A, diagonal_attack, orthogonal_attack;
  int32_t white_king_danger, black_king_danger;
  uint64_t white_minor_guarded, black_minor_guarded;
  uint64_t white_mobility_safe, black_mobility_safe;
  uint64_t white_xray_ok, black_xray_ok;
  uint64_t T, white_pawn_attack, black_pawn_attack;
  int opening, endgame;
  uint8_t strong_attack_white, strong_attack_black;
  uint8_t token;
  int value, positional;

  pawn_value = pawn_hash_table + (position->pawn_hash_key & (1 << 16) - 1);
  PREFETCH(pawn_value)
  index = position->material >> 8 & 0x7ffff;
  token = material_table[index].token;
  position->flag = material_table[index].flag;

  if (!(position->material & 0x80000000))
    material_value = material_table[index].value;
  else {
    if (popcnt(board.piece[white_queen]) > 1
      || popcnt(board.piece[black_queen]) > 1
      || popcnt(board.piece[white_rook]) > 2
      || popcnt(board.piece[black_rook]) > 2
      || popcnt(board.piece[white_king_bishop]) > 1
      || popcnt(board.piece[black_queen_bishop]) > 1
      || popcnt(board.piece[white_queen_bishop]) > 1
      || popcnt(board.piece[black_king_bishop]) > 1
      || popcnt(board.piece[white_knight]) > 2
      || popcnt(board.piece[black_knight]) > 2) {
      token = 0x80;
      material_value = eval_material();
      position->flag = 0;

      if (board.piece[white_queen] | board.piece[white_rook]
        | (board.piece[white_king_bishop] | board.piece[white_queen_bishop])
        | board.piece[white_knight])
        position->flag |= 2;

      if (board.piece[black_queen] | board.piece[black_rook]
        | (board.piece[black_queen_bishop] | board.piece[black_king_bishop])
        | board.piece[black_knight])
        position->flag |= 1;
    }
    else {
      material_value = material_table[index].value;
      position->material &= 0x7fffffff;
    }
  }

  if (((position->hash_key
    ^ eval_hash[position->hash_key & 0x8000 - 1]) & 0xffffffffffff0000) == 0) {
    score = static_cast<int>(static_cast<int16_t>(eval_hash[position->hash_key & 0x8000 - 1] & 0xffff));
    position->lazy = 0;
    eval_mobility();
    position->positional_value = (board.white_to_move ? score : -score) - material_value;
    position->score = score;

    if (move && !(position - 1)->lazy)
      board.white_to_move ? update_white_gain(move) : update_black_gain(move);
    return;
  }

  if ((position->material & 0xff) == 0) {
    board.white_to_move
      ? endgame_pawn_white(material_value, token, pawn_value)
      : endgame_pawn_black(material_value, token, pawn_value);
    return;
  }

  if (board.white_to_move) {
    positional = (position - 1)->positional_value;
    capture = position->capture;
    to = move & 077;
    score = pst[board.square[to]][to] - pst[board.square[to]][(move >> 6 & 077)];

    if (capture)
      score -= pst[capture][to];
    phase = position->material & 0xff;
    endgame = static_cast<int16_t>(score & 0xffff);
    opening = (endgame < 0) + static_cast<int16_t>(score >> 16 & 0xffff);
    anti_phase = 32 - phase;
    score = (endgame * anti_phase + opening * phase) / 32;
    positional += score;
    value = positional + material_value;

    if (value < -max - 16 * static_cast<int>((position - 1)->lazy) || value > -min + 16 * static_cast<int>((position -
      1)->lazy)) {
      position->lazy = (position - 1)->lazy + 1;
      position->score = value;
      position->positional_value = positional;
      eval_mobility();
      return;
    }
  }
  else {
    positional = (position - 1)->positional_value;
    capture = position->capture;
    to = move & 077;
    score = pst[board.square[to]][to] - pst[board.square[to]][(move >> 6 & 077)];

    if (capture)
      score -= pst[capture][to];
    phase = position->material & 0xff;
    endgame = static_cast<int16_t>(score & 0xffff);
    opening = (endgame < 0) + static_cast<int16_t>(score >> 16 & 0xffff);
    anti_phase = 32 - phase;
    score = (endgame * anti_phase + opening * phase) / 32;
    positional += score;
    value = positional + material_value;

    if (value < min - 16 * static_cast<int>((position - 1)->lazy) || value > max + 16 * static_cast<int>((position - 1)
      ->lazy)) {
      position->lazy = (position - 1)->lazy + 1;
      position->score = -value;
      position->positional_value = positional;
      eval_mobility();
      return;
    }
  }
  white_king_square = board.white_king;
  black_king_square = board.black_king;
  white_king_attack = attack_king[white_king_square];
  black_king_attack = attack_king[black_king_square];
  strong_attack_black = strong_attack_white = 0;

  if (pawn_value->pawn_hash_key != position->pawn_hash_key)
    eval_pawns(pawn_value);
  score = position->pst_value + pawn_value->score;
  position->white_xray = 0;
  A = (board.piece[white_pawn] & ~0x0101010101010101) << 7;
  T = A & board.piece[black_king];
  position->black_king_check = T >> 7;
  white_pawn_attack = A;
  A = (board.piece[white_pawn] & ~0x8080808080808080) << 9;
  T = A & board.piece[black_king];
  position->black_king_check |= T >> 9;
  white_pawn_attack |= A;
  position->white_attack = white_pawn_attack;
  A = (board.piece[black_pawn] & ~0x8080808080808080) >> 7;
  T = A & board.piece[white_king];
  position->white_king_check = T << 7;
  black_pawn_attack = A;
  A = (board.piece[black_pawn] & ~0x0101010101010101) >> 9;
  T = A & board.piece[white_king];
  position->white_king_check |= T << 9;
  black_pawn_attack |= A;
  position->black_attack = black_pawn_attack;

  black_xray_ok = ~board.piece[black_pawn] & ~white_pawn_attack;
  white_xray_ok = ~board.piece[white_pawn] & ~black_pawn_attack;
  white_minor_guarded = (board.piece[white_knight] | (board.piece[white_king_bishop]
    | board.piece[white_queen_bishop])) & white_pawn_attack;
  black_minor_guarded = (board.piece[black_knight] | (board.piece[black_queen_bishop]
    | board.piece[black_king_bishop])) & black_pawn_attack;

  if (white_pawn_attack & black_king_attack)
    black_king_danger = (1 << 16) + 0;
  else
    black_king_danger = 0;
  U = board.occupied_total >> 8 & board.piece[white_pawn];

  while (U) {
    b = bsf(U);
    score -= (3 << 16) + 10;
    U &= U - 1;
  }
  white_mobility_safe = ~(black_pawn_attack | board.piece[occupied_white]);

  U = board.piece[white_queen];

  while (U) {
    b = bsf(U);
    U &= U - 1;
    diagonal_attack =
      bitboard_line_obscured[1][b][board.occupied_45_right >> line_turn[1][b] & 077]
      | bitboard_line_obscured[0][b][board.occupied_45_left >> line_turn[0][b] & 077];
    orthogonal_attack =
      bitboard_line_obscured[2][b][board.occupied_total >> line_turn[2][b] & 077]
      | bitboard_line_obscured[3][b][board.occupied_90_left >> line_turn[3][b] & 077];

    if (board.piece[black_king] & diagonal[b]) {
      T = (bitboard_line_obscured[1][black_king_square][board.occupied_45_right
          >> line_turn[1][black_king_square] & 077]
        | bitboard_line_obscured[0][black_king_square][board.occupied_45_left
          >> line_turn[0][black_king_square] & 077]) & diagonal_attack;

      if (T) {
        score += white_queen_xray_diagonal[board.square[bsf(T)]];
        position->white_xray |= T;
        xray_table[bsf(T)] = b;
      }
    }
    else if (board.piece[black_king] & orthogonal[b]) {
      T = (bitboard_line_obscured[2][black_king_square][board.occupied_total
          >> line_turn[2][black_king_square] & 077]
        | bitboard_line_obscured[3][black_king_square][board.occupied_90_left
          >> line_turn[3][black_king_square] & 077]) & orthogonal_attack;

      if (T) {
        score += white_queen_xray_orthogonal[board.square[bsf(T)]];
        position->white_xray |= T;
        xray_table[bsf(T)] = b;
      }
    }
    A = diagonal_attack | orthogonal_attack;
    T = A & white_mobility_safe;
    position->white_attack |= A;

    if (A & black_king_attack)
      black_king_danger += (1 << 16) + 40;

    if (A & board.piece[black_king])
      position->black_king_check |= square_set[b];

    if (A & white_king_attack)
      score += (5 << 16) + 2;
    score += ((2 << 16) + 2) * popcnt(T);

    if (A & ~black_pawn_attack & board.piece[occupied_black])
      score += (4 << 16) + 4;

    if (board.piece[black_pawn] & attack_pawn_black[b]) {
      score -= (8 << 16) + 12;
      strong_attack_black += 1;
    }

    if (b >> 3 == rank_7) {
      if ((board.piece[black_pawn]
        | board.piece[black_king]) & 0xffff000000000000) {
        score += (5 << 16) + 25;

        if (board.piece[white_rook] & 0x00ff000000000000 & orthogonal_attack
          && board.piece[black_king] & 0xff00000000000000)
          score += (10 << 16) + 15;
      }
    }
  }

  U = board.piece[white_rook];

  while (U) {
    b = bsf(U);
    U &= U - 1;
    A = bitboard_line_obscured[2][b][board.occupied_total >> line_turn[2][b] & 077]
      | bitboard_line_obscured[3][b][board.occupied_90_left >> line_turn[3][b] & 077];
    position->white_attack |= A;

    if (board.piece[black_king] & orthogonal[b]) {
      T = (bitboard_line_obscured[2][black_king_square][board.occupied_total
          >> line_turn[2][black_king_square] & 077]
        | bitboard_line_obscured[3][black_king_square][board.occupied_90_left
          >> line_turn[3][black_king_square] & 077]) & A;

      if (T) {
        score += white_rook_xray[board.square[bsf(T)]];
        position->white_xray |= T;
        xray_table[bsf(T)] = b;
      }
    }

    if (A & black_king_attack)
      black_king_danger += (1 << 16) + 25;

    if (A & board.piece[black_king])
      position->black_king_check |= square_set[b];

    if (A & white_king_attack)
      score += (3 << 16) + 1;
    score += ((2 << 16) + 3) * popcnt(A & white_xray_ok);

    if (A & ~black_pawn_attack & board.piece[black_pawn])
      score += (2 << 16) + 3;

    if (A & ((board.piece[black_knight] | (board.piece[black_queen_bishop]
      | board.piece[black_king_bishop])) & ~black_pawn_attack))
      score += (4 << 16) + 5;

    if (A & board.piece[black_queen]) {
      score += (5 << 16) + 5;
      strong_attack_white += 1;
    }

    if (board.piece[black_pawn] & attack_pawn_black[b]) {
      score -= (7 << 16) + 10;
      strong_attack_black += 1;
    }

    if ((board.piece[white_pawn] & open_file_white[b]) == 0) {
      score += (3 << 16) + 6;

      if ((board.piece[black_pawn] & open_file_white[b]) == 0) {
        T = black_minor_guarded & open_file_white[b];

        if (!T)
          score += (20 << 16) + 10;
        else {
          if (int t = bsf(T); (files_isolated[(t & 7)] & in_front_black[(t
            >> 3)] & board.piece[white_pawn]) == 0)
            score += (10 << 16) + 0;
          else
            score += (15 << 16) + 5;
        }
      }
      else {
        T = open_file_white[b] & board.piece[black_pawn];

        if (T) {
          if (int t = bsf(T); (files_isolated[(t & 7)] & in_front_white[(t
            >> 3)] & board.piece[black_pawn]) == 0)
            score += (5 << 16) + 5;
        }
      }

      if (board.piece[black_king] & open_file_white[b])
        score += (15 << 16) + 0;
    }

    if (square_set[b] & 0x00007e7e7e000000 && (files_isolated[(b & 7)] & in_front_white[(b >> 3)]
      & board.piece[black_pawn]) == 0) {
      if (board.piece[white_pawn] & attack_pawn_white[b]) {
        score += (1 << 16) + 2;

        if (A & (black_king_attack
          | (board.piece[occupied_black] & ~black_pawn_attack)) & rank_table[
          (b >> 3)])
          score += (3 << 16) + 4;
      }
    }

    if (b >> 3 == rank_8) {
      if (board.piece[black_king] & 0xff00000000000000)
        score += (5 << 16) + 10;
    }

    if (b >> 3 == rank_7) {
      if ((board.piece[black_pawn]
        | board.piece[black_king]) & 0xffff000000000000) {
        score += (10 << 16) + 30;

        if (board.piece[black_king] & 0xff00000000000000
          && (board.piece[white_queen]
            | board.piece[white_rook]) & 0x00ff000000000000 & A)
          score += (10 << 16) + 20;
      }
    }

    if (b >> 3 == rank_6 && (board.piece[black_pawn]
      | board.piece[black_king]) & 0xffffff0000000000)
      score += (5 << 16) + 15;
  }

  white_mobility_safe |= board.piece[occupied_black] ^ board.piece[black_pawn];

  U = board.piece[white_king_bishop] | board.piece[white_queen_bishop];

  while (U) {
    b = bsf(U);
    U &= U - 1;
    A = bitboard_line_obscured[1][b][board.occupied_45_right >> line_turn[1][b] & 077]
      | bitboard_line_obscured[0][b][board.occupied_45_left >> line_turn[0][b] & 077];
    position->white_attack |= A;

    if (board.piece[black_king] & diagonal[b]) {
      T = (bitboard_line_obscured[1][black_king_square][board.occupied_45_right
          >> line_turn[1][black_king_square] & 077]
        | bitboard_line_obscured[0][black_king_square][board.occupied_45_left
          >> line_turn[0][black_king_square] & 077]) & A;

      if (T) {
        score += white_bishop_xray[board.square[bsf(T)]];
        position->white_xray |= T;
        xray_table[bsf(T)] = b;
      }
    }

    if (A & black_king_attack)
      black_king_danger += (1 << 16) + 15;

    if (A & board.piece[black_king])
      position->black_king_check |= square_set[b];

    if (A & white_king_attack)
      score += (2 << 16) + 1;
    score += ((5 << 16) + 5) * popcnt(A & white_mobility_safe & in_front_white[(b >> 3)]);

    if (A & ~black_pawn_attack & board.piece[black_pawn])
      score += (3 << 16) + 4;

    if (A & ~black_pawn_attack & board.piece[black_knight])
      score += (5 << 16) + 5;

    if (A & (board.piece[black_rook] | board.piece[black_queen])) {
      score += (7 << 16) + 10;
      strong_attack_white += 1;
    }

    if (board.piece[black_pawn] & attack_pawn_black[b]) {
      score -= (5 << 16) + 7;
      strong_attack_black += 1;
    }

    if (square_set[b] & 0x55aa55aa55aa55aa) {
      score -= (pawn_value->white_pawn_white + pawn_value->black_pawn_white / 2) * ((1 << 16) + 1);
      score += popcnt(board.piece[black_pawn] & 0x55aa55aa55aa55aa & in_front_black[(b
        >> 3)] & ~black_pawn_attack) * ((0 << 16) + 2);
    }
    else {
      score -= (pawn_value->white_pawn_black + pawn_value->black_pawn_black / 2) * ((1 << 16) + 1);
      score += popcnt(board.piece[black_pawn] & 0xaa55aa55aa55aa55 & in_front_black[(b
        >> 3)] & ~black_pawn_attack) * ((0 << 16) + 2);
    }

    if (square_set[b] & 0x00007e7e7e000000 && (files_isolated[(b & 7)] & in_front_white[(b
      >> 3)] & board.piece[black_pawn]) == 0) {
      if (board.piece[white_pawn] & attack_pawn_white[b]) {
        score += (1 << 16) + 2;

        if (A & (black_king_attack
          | (board.piece[occupied_black] & ~black_pawn_attack)))
          score += (3 << 16) + 4;
      }
    }

    if (board.square[trapped_bishop_squares[b]] == black_pawn) {
      score -= (40 << 16) + 40;

      if (board.square[trapped_bishop_squares_protected[b]] == black_pawn)
        score -= (40 << 16) + 40;
    }
  }
  U = board.piece[white_knight];

  while (U) {
    b = bsf(U);
    U &= U - 1;
    A = attack_knight[b];
    position->white_attack |= A;

    if (A & (black_king_attack | board.piece[black_king]))
      black_king_danger += (1 << 16) + 15;

    if (A & board.piece[black_king])
      position->black_king_check |= square_set[b];

    if (A & (white_king_attack | board.piece[white_king]))
      score += (4 << 16) + 2;
    score += ((6 << 16) + 8) * popcnt(A & white_mobility_safe & in_front_white[(b >> 3)]);

    if (A & ~black_pawn_attack & board.piece[black_pawn])
      score += (3 << 16) + 4;

    if (A & ~black_pawn_attack & (board.piece[black_queen_bishop] | board.piece[black_king_bishop]))
      score += (5 << 16) + 5;

    if (A & (board.piece[black_rook] | board.piece[black_queen])) {
      score += (7 << 16) + 10;
      strong_attack_white += 1;
    }

    if (board.piece[black_pawn] & attack_pawn_black[b]) {
      score -= (5 << 16) + 7;
      strong_attack_black += 1;
    }

    if (square_set[b] & 0x00007e7e7e000000 && (files_isolated[(b & 7)] & in_front_white[(b >> 3)]
      & board.piece[black_pawn]) == 0) {
      score += (2 << 16) + 3;

      if (board.piece[white_pawn] & attack_pawn_white[b]) {
        score += (2 << 16) + 3;

        if (A & (black_king_attack
          | (board.piece[occupied_black] & ~black_pawn_attack))) {
          score += (5 << 16) + 5;

          if (b >> 3 == rank_5)
            score += (2 << 16) + 2;

          if ((b & 7) == files_d || (b & 7) == files_e)
            score += (3 << 16) + 3;
        }
      }
    }
  }

  if (black_pawn_attack & white_king_attack)
    white_king_danger = (1 << 16) + 0;
  else
    white_king_danger = 0;
  U = board.occupied_total << 8 & board.piece[black_pawn];
  position->black_xray = 0;

  while (U) {
    b = bsf(U);
    score += (3 << 16) + 10;
    U &= U - 1;
  }
  black_mobility_safe = ~(white_pawn_attack | board.piece[occupied_black]);

  U = board.piece[black_queen];

  while (U) {
    b = bsf(U);
    U &= U - 1;
    diagonal_attack = bitboard_line_obscured[1][b][board.occupied_45_right >> line_turn[1][b] & 077]
      | bitboard_line_obscured[0][b][board.occupied_45_left >> line_turn[0][b] & 077];
    orthogonal_attack = bitboard_line_obscured[2][b][board.occupied_total >> line_turn[2][b] & 077]
      | bitboard_line_obscured[3][b][board.occupied_90_left >> line_turn[3][b] & 077];

    if (board.piece[white_king] & diagonal[b]) {
      T = (bitboard_line_obscured[1][white_king_square][board.occupied_45_right
          >> line_turn[1][white_king_square] & 077]
        | bitboard_line_obscured[0][white_king_square][board.occupied_45_left
          >> line_turn[0][white_king_square] & 077]) & diagonal_attack;

      if (T) {
        score -= black_queen_xray_diagonal[board.square[bsf(T)]];
        position->black_xray |= T;
        xray_table[bsf(T)] = b;
      }
    }
    else if (board.piece[white_king] & orthogonal[b]) {
      T = (bitboard_line_obscured[2][white_king_square][board.occupied_total
          >> line_turn[2][white_king_square] & 077]
        | bitboard_line_obscured[3][white_king_square][board.occupied_90_left
          >> line_turn[3][white_king_square] & 077]) & orthogonal_attack;

      if (T) {
        score -= black_queen_xray_orthogonal[board.square[bsf(T)]];
        position->black_xray |= T;
        xray_table[bsf(T)] = b;
      }
    }
    A = diagonal_attack | orthogonal_attack;
    T = A & black_mobility_safe;
    position->black_attack |= A;

    if (A & white_king_attack)
      white_king_danger += (1 << 16) + 40;

    if (A & board.piece[white_king])
      position->white_king_check |= square_set[b];

    if (A & black_king_attack)
      score -= (5 << 16) + 2;
    score -= ((2 << 16) + 2) * popcnt(T);

    if (A & ~white_pawn_attack & board.piece[occupied_white])
      score -= (4 << 16) + 4;

    if (board.piece[white_pawn] & attack_pawn_white[b]) {
      score += (8 << 16) + 12;
      strong_attack_white += 1;
    }

    if (b >> 3 == rank_2) {
      if ((board.piece[white_pawn]
        | board.piece[white_king]) & 0x000000000000ffff) {
        score -= (5 << 16) + 25;

        if (board.piece[black_rook] & 0x000000000000ff00 & orthogonal_attack
          && board.piece[white_king] & 0x00000000000000ff)
          score -= (10 << 16) + 15;
      }
    }
  }

  U = board.piece[black_rook];

  while (U) {
    b = bsf(U);
    U &= U - 1;
    A = bitboard_line_obscured[2][b][board.occupied_total >> line_turn[2][b] & 077]
      | bitboard_line_obscured[3][b][board.occupied_90_left >> line_turn[3][b] & 077];
    position->black_attack |= A;

    if (board.piece[white_king] & orthogonal[b]) {
      T = A & (bitboard_line_obscured[2][white_king_square][board.occupied_total
          >> line_turn[2][white_king_square] & 077]
        | bitboard_line_obscured[3][white_king_square][board.occupied_90_left
          >> line_turn[3][white_king_square] & 077]);

      if (T) {
        score -= black_rook_xray[board.square[bsf(T)]];
        position->black_xray |= T;
        xray_table[bsf(T)] = b;
      }
    }

    if (A & white_king_attack)
      white_king_danger += (1 << 16) + 25;

    if (A & board.piece[white_king])
      position->white_king_check |= square_set[b];

    if (A & black_king_attack)
      score -= (3 << 16) + 1;
    score -= ((2 << 16) + 3) * popcnt(A & black_xray_ok);

    if (A & ~white_pawn_attack & board.piece[white_pawn])
      score -= (2 << 16) + 3;

    if (A & (board.piece[white_knight] | (board.piece[white_king_bishop] | board.piece[white_queen_bishop])) & ~
      white_pawn_attack)
      score -= (4 << 16) + 5;

    if (A & board.piece[white_queen]) {
      score -= (5 << 16) + 5;
      strong_attack_black += 1;
    }

    if (board.piece[white_pawn] & attack_pawn_white[b]) {
      score += (7 << 16) + 10;
      strong_attack_white += 1;
    }

    if ((board.piece[black_pawn] & open_file_black[b]) == 0) {
      score -= (3 << 16) + 6;

      if ((board.piece[white_pawn] & open_file_black[b]) == 0) {
        T = white_minor_guarded & open_file_black[b];

        if (!T)
          score -= (20 << 16) + 10;
        else {
          if (int t = bsr(T); (files_isolated[(t & 7)] & in_front_white[(t
            >> 3)] & board.piece[black_pawn]) == 0)
            score -= (10 << 16) + 0;
          else
            score -= (15 << 16) + 5;
        }
      }
      else {
        T = open_file_black[b] & board.piece[white_pawn];

        if (T) {
          if (int t = bsr(T); (files_isolated[(t & 7)] & in_front_black[(t
            >> 3)] & board.piece[white_pawn]) == 0)
            score -= (5 << 16) + 5;
        }
      }

      if (board.piece[white_king] & open_file_black[b])
        score -= (15 << 16) + 0;
    }

    if (square_set[b] & 0x0000007e7e7e0000 && (files_isolated[(b & 7)] & in_front_black[(b
      >> 3)] & board.piece[white_pawn]) == 0) {
      if (board.piece[black_pawn] & attack_pawn_black[b]) {
        score -= (1 << 16) + 2;

        if (A & (white_king_attack
          | (board.piece[occupied_white] & ~white_pawn_attack)) & rank_table[
          (b >> 3)])
          score -= (3 << 16) + 4;
      }
    }

    if (b >> 3 == rank_1) {
      if (board.piece[white_king] & 0x00000000000000ff)
        score -= (5 << 16) + 10;
    }

    if (b >> 3 == rank_2) {
      if ((board.piece[white_pawn]
        | board.piece[white_king]) & 0x000000000000ffff) {
        score -= (10 << 16) + 30;

        if (board.piece[white_king] & 0x00000000000000ff
          && (board.piece[black_queen]
            | board.piece[black_rook]) & 0x000000000000ff00 & A)
          score -= (10 << 16) + 20;
      }
    }

    if (b >> 3 == rank_3 && (board.piece[white_pawn]
      | board.piece[white_king]) & 0x00000000000ffffff)
      score -= (5 << 16) + 15;
  }

  black_mobility_safe |=
    board.piece[occupied_white] ^ board.piece[white_pawn];
  U = board.piece[black_queen_bishop] | board.piece[black_king_bishop];

  while (U) {
    b = bsf(U);
    U &= U - 1;
    A = bitboard_line_obscured[1][b][board.occupied_45_right >> line_turn[1][b] & 077]
      | bitboard_line_obscured[0][b][board.occupied_45_left >> line_turn[0][b] & 077];
    position->black_attack |= A;

    if (board.piece[white_king] & diagonal[b]) {
      T = A & (bitboard_line_obscured[1][white_king_square][board.occupied_45_right
          >> line_turn[1][white_king_square] & 077]
        | bitboard_line_obscured[0][white_king_square][board.occupied_45_left
          >> line_turn[0][white_king_square] & 077]);

      if (T) {
        score -= black_bishop_xray[board.square[bsf(T)]];
        position->black_xray |= T;
        xray_table[bsf(T)] = b;
      }
    }

    if (A & white_king_attack)
      white_king_danger += (1 << 16) + 15;

    if (A & board.piece[white_king])
      position->white_king_check |= square_set[b];

    if (A & black_king_attack)
      score -= (2 << 16) + 1;
    score -= ((5 << 16) + 5) * popcnt(A & black_mobility_safe & in_front_black[(b >> 3)]);

    if (A & ~white_pawn_attack & board.piece[white_pawn])
      score -= (3 << 16) + 4;

    if (A & ~white_pawn_attack & board.piece[white_knight])
      score -= (5 << 16) + 5;

    if (A & (board.piece[white_rook] | board.piece[white_queen])) {
      score -= (7 << 16) + 10;
      strong_attack_black += 1;
    }

    if (board.piece[white_pawn] & attack_pawn_white[b]) {
      score += (5 << 16) + 7;
      strong_attack_white += 1;
    }

    if (square_set[b] & 0x55aa55aa55aa55aa) {
      score += (pawn_value->black_pawn_white + pawn_value->white_pawn_white / 2) * ((1 << 16) + 1);
      score -= popcnt(board.piece[white_pawn] & 0x55aa55aa55aa55aa & in_front_white[(b
        >> 3)] & ~white_pawn_attack) * ((0 << 16) + 2);
    }
    else {
      score += (pawn_value->black_pawn_black + pawn_value->white_pawn_black / 2) * ((1 << 16) + 1);
      score -= popcnt(board.piece[white_pawn] & 0xaa55aa55aa55aa55 & in_front_white[(b
        >> 3)] & ~white_pawn_attack) * ((0 << 16) + 2);
    }

    if (square_set[b] & 0x0000007e7e7e0000 && (files_isolated[(b & 7)] & in_front_black[(b
      >> 3)] & board.piece[white_pawn]) == 0) {
      if (board.piece[black_pawn] & attack_pawn_black[b]) {
        score -= (1 << 16) + 2;

        if (A & (white_king_attack
          | (board.piece[occupied_white] & ~white_pawn_attack)))
          score -= (3 << 16) + 4;
      }
    }

    if (board.square[trapped_bishop_squares[b]] == white_pawn) {
      score += (40 << 16) + 40;

      if (board.square[trapped_bishop_squares_protected[b]] == white_pawn)
        score += (40 << 16) + 40;
    }
  }

  U = board.piece[black_knight];

  while (U) {
    b = bsf(U);
    U &= U - 1;
    A = attack_knight[b];
    position->black_attack |= A;

    if (A & (white_king_attack | board.piece[white_king]))
      white_king_danger += (1 << 16) + 15;

    if (A & board.piece[white_king])
      position->white_king_check |= square_set[b];

    if (A & (black_king_attack | board.piece[black_king]))
      score -= (4 << 16) + 2;
    score -= ((6 << 16) + 8) * popcnt(A & black_mobility_safe & in_front_black[(b >> 3)]);

    if (A & ~white_pawn_attack & board.piece[white_pawn])
      score -= (3 << 16) + 4;

    if (A & ~white_pawn_attack & (board.piece[white_king_bishop] | board.piece[white_queen_bishop]))
      score -= (5 << 16) + 5;

    if (A & (board.piece[white_rook] | board.piece[white_queen])) {
      score -= (7 << 16) + 10;
      strong_attack_black += 1;
    }

    if (board.piece[white_pawn] & attack_pawn_white[b]) {
      score += (5 << 16) + 7;
      strong_attack_white += 1;
    }

    if (square_set[b] & 0x0000007e7e7e0000 && (files_isolated[(b & 7)] & in_front_black[(b >> 3)]
      & board.piece[white_pawn]) == 0) {
      score -= (2 << 16) + 3;

      if (board.piece[black_pawn] & attack_pawn_black[b]) {
        score -= (2 << 16) + 3;

        if (A & (white_king_attack
          | (board.piece[occupied_white] & ~white_pawn_attack))) {
          score -= (5 << 16) + 5;

          if (b >> 3 == rank_4)
            score -= (2 << 16) + 2;

          if ((b & 7) == files_d || (b & 7) == files_e)
            score -= (3 << 16) + 3;
        }
      }
    }
  }

  position->white_attack |= white_king_attack;
  position->black_attack |= black_king_attack;

  if (black_king_attack & board.piece[white_king]) {
    position->white_king_check |= square_set[board.black_king];
    position->black_king_check |= square_set[board.white_king];
  }

  if (~position->black_attack & white_king_attack & board.piece[black_pawn])
    score += (0 << 16) + 5;
  T = trapped_rook_squares[white_king_square] & board.piece[white_rook];

  if (T) {
    int t = bsf(T);
    T = open_file_white[t] & board.piece[white_pawn];

    if (T) {
      t = bsf(T);
      t >>= 3;
      score -= (10 * (6 - t) << 16) + 0;
    }
  }

  if (white_king_attack & black_king_attack)
    white_king_danger += (0 << 16) + 0;

  if (board.piece[black_queen]) {
    score -= (king_danger_weight[white_king_danger >> 16] * (white_king_danger & 0xffff) / 8 << 16)
      + pawn_value->white_king_danger;
  }

  if (~position->white_attack & black_king_attack & board.piece[white_pawn])
    score -= (0 << 16) + 5;
  T = trapped_rook_squares[black_king_square] & board.piece[black_rook];

  if (T) {
    int t = bsr(T);
    T = open_file_black[t] & board.piece[black_pawn];

    if (T) {
      t = bsr(T);
      t >>= 3;
      score += (10 * (t - 1) << 16) + 0;
    }
  }

  if (white_king_attack & black_king_attack)
    black_king_danger += (0 << 16) + 0;

  if (board.piece[white_queen]) {
    score += (king_danger_weight[black_king_danger >> 16] * (black_king_danger & 0xffff) / 8 << 16)
      + pawn_value->black_king_danger;
  }

  if (strong_attack_white >= 2)
    score += (15 << 16) + 25;

  if (strong_attack_black >= 2)
    score -= (15 << 16) + 25;

  if ((board.piece[white_rook]
    | board.piece[white_queen]) & rook_traps_king[(black_king_square & 7)]) {
    score += (0 << 16) + 5;

    if ((rook_traps_king[(black_king_square & 7)] & (board.piece[white_pawn]
      | board.piece[black_pawn])) == 0)
      score += (5 << 16) + 15;
  }

  if ((board.piece[black_rook]
    | board.piece[black_queen]) & rook_traps_king[(white_king_square & 7)]) {
    score -= (0 << 16) + 5;

    if ((rook_traps_king[(white_king_square & 7)] & (board.piece[black_pawn]
      | board.piece[white_pawn])) == 0)
      score -= (5 << 16) + 15;
  }

  U = pawn_value->white_passed_pawn_file;

  while (U) {
    b = bsr(file_table[bsf(U)] & board.piece[white_pawn]);
    U &= U - 1;
    rank = b >> 3;

    if (rank <= rank_3)
      continue;

    if ((position->flag & 28) == 8) {
      if (board.piece[white_rook] & open_file_white[b]) {
        if (rank == rank_7)
          score -= (20 << 16) + 50;

        else if (rank == rank_6)
          score -= (0 << 16) + 15;
      }

      if (open_file_white[b] & board.piece[white_king]
        && rook_traps_king[(white_king_square & 7)] & board.piece[black_rook])
        score -= (((0) << 16) + (1 << (rank - rank_2)));
    }

    if (board.square[b + 8] == 0)
      score += passed_pawn_is_mobile_value[rank];

    if ((open_file_white[b] & board.piece[occupied_white]) == 0)
      score += my_passed_pawn_clear_value[rank];

    if ((open_file_white[b] & board.piece[occupied_black]) == 0)
      score += opp_passed_pawn_clear_value[rank];

    if ((open_file_white[b] & ~position->white_attack & position->black_attack) == 0)
      score += passed_pawn_is_free_value[rank];

    if ((position->flag & 28) == 4) {
      if (rank == rank_7
        && board.piece[white_queen] & open_file_white[b])
        score -= (0 << 16) + 10;
      score += queen_rank_endgame_value[rank];
    }
  }
  U = pawn_value->black_passed_pawn_file;

  while (U) {
    b = bsf(file_table[bsf(U)] & board.piece[black_pawn]);
    U &= U - 1;
    rank = b >> 3;

    if (rank >= rank_6)
      continue;

    if ((position->flag & 28) == 8) {
      if (board.piece[black_rook] & open_file_black[b]) {
        if (rank == rank_2)
          score += (20 << 16) + 50;

        else if (rank == rank_3)
          score += (0 << 16) + 15;
      }

      if (open_file_black[b] & board.piece[black_king]
        && rook_traps_king[(black_king_square & 7)] & board.piece[white_rook])
        score += (((0) << 16) + (1 << (rank_7 - rank)));
    }

    if (board.square[b - 8] == 0)
      score -= passed_pawn_is_mobile_value[7 - rank];

    if ((open_file_black[b] & board.piece[occupied_black]) == 0)
      score -= my_passed_pawn_clear_value[7 - rank];

    if ((open_file_black[b] & board.piece[occupied_white]) == 0)
      score -= opp_passed_pawn_clear_value[7 - rank];

    if ((open_file_black[b] & position->white_attack & ~position->black_attack) == 0)
      score -= passed_pawn_is_free_value[7 - rank];

    if ((position->flag & 28) == 4) {
      if (rank == rank_2
        && board.piece[black_queen] & open_file_black[b])
        score += (0 << 16) + 10;
      score -= queen_rank_endgame_value[7 - rank];
    }
  }

  phase = position->material & 0xff;
  endgame = static_cast<int16_t>(score & 0xffff);
  opening = (endgame < 0) + static_cast<int16_t>(score >> 16 & 0xffff);
  anti_phase = 32 - phase;
  score = endgame * anti_phase + opening * phase;
  score = score / 32 + material_value;
  score = score * token / 128;

  if (score > 0)
    score -= pawn_value->white_draw_weight * (score <= 100 ? score : 100) / 64;
  else
    score += pawn_value->black_draw_weight * (-score <= 100 ? -score : 100) / 64;

  if (position->reversible > 50) {
    score *= 114 - position->reversible;
    score /= 64;
  }

  if (score > 0) {
    if (position->flag & 32) {
      if (board.piece[white_knight]) {
        if (board.piece[white_pawn] == square_set[a7]
          && (board.piece[black_king]
            | attack_king[board.black_king]) & square_set[a8])
          score = 0;

        if (board.piece[white_pawn] == square_set[h7]
          && (board.piece[black_king]
            | attack_king[board.black_king]) & square_set[h8])
          score = 0;
      }
      else if (board.piece[white_king_bishop]
        && !(board.piece[white_pawn] & 0x7f7f7f7f7f7f7f7f)
        && (board.piece[black_king]
          | attack_king[board.black_king]) & square_set[h8]) {
        if (board.piece[white_pawn] & square_set[h5]
          && board.piece[black_pawn] == (square_set[g7] | square_set[h6])) {
        }
        else
          score = 0;
      }
      else if (board.piece[white_queen_bishop]
        && !(board.piece[white_pawn] & 0xfefefefefefefefe)
        && (board.piece[black_king]
          | attack_king[board.black_king]) & square_set[a8]) {
        if (board.piece[white_pawn] & square_set[a5]
          && board.piece[black_pawn] == (square_set[b7] | square_set[a6])) {
        }
        else
          score = 0;
      }

      if (!board.piece[white_pawn])
        score = 0;
    }
  }
  else {
    if (position->flag & 64) {
      if (board.piece[black_knight]) {
        if (board.piece[black_pawn] == square_set[a2]
          && (board.piece[white_king]
            | attack_king[board.white_king]) & square_set[a1])
          score = 0;

        if (board.piece[black_pawn] == square_set[h2]
          && (board.piece[white_king]
            | attack_king[board.white_king]) & square_set[h1])
          score = 0;
      }
      else if (board.piece[black_king_bishop]
        && !(board.piece[black_pawn] & 0x7f7f7f7f7f7f7f7f)
        && (board.piece[white_king]
          | attack_king[board.white_king]) & square_set[h1]) {
        if (board.piece[black_pawn] & square_set[h4]
          && board.piece[white_pawn] == (square_set[g2] | square_set[h3])) {
        }
        else
          score = 0;
      }
      else if (board.piece[black_queen_bishop]
        && !(board.piece[black_pawn] & 0xfefefefefefefefe)
        && (board.piece[white_king]
          | attack_king[board.white_king]) & square_set[a1]) {
        if (board.piece[black_pawn] & square_set[a4]
          && board.piece[white_pawn] == (square_set[b2] | square_set[a3])) {
        }
        else
          score = 0;
      }

      if (!board.piece[black_pawn])
        score = 0;
    }
  }

  position->score = board.white_to_move ? score : -score;
  position->positional_value = score - material_value;
  position->lazy = 0;
  eval_hash[position->hash_key & 0x8000 - 1] =
    (position->hash_key & 0xffffffffffff0000) | (position->score & 0xffff);

  if (move && !(position - 1)->lazy)
    board.white_to_move ? update_white_gain(move) : update_black_gain(move);
}

void init_pawns() {
  int file, rank;
  constexpr int ch[8] =
    {
      files_b, files_b, files_c, files_d, files_e, files_f, files_g, files_g
    };

  constexpr int change[8] =
    {
      1, 1, 1, 1, -1, -1, -1, -1
    };

  for (file = files_a; file <= files_h; file++) {
    shelter_storm[file].edge = file_table[ch[file] - change[file]];
    shelter_storm[file].middle = file_table[ch[file]];
    shelter_storm[file].center = file_table[ch[file] + change[file]];
  }

  for (rank = rank_1; rank <= rank_8; rank++) {
    shelter_storm[files_a].shelter_edge[rank] = aa_shelter[rank];
    shelter_storm[files_a].storm_edge[rank] = aa_storm[rank];
    shelter_storm[files_a].shelter_middle[rank] = ab_shelter[rank];
    shelter_storm[files_a].storm_middle[rank] = ab_storm[rank];
    shelter_storm[files_a].shelter_center[rank] = ac_shelter[rank];
    shelter_storm[files_a].storm_center[rank] = ac_storm[rank];
    shelter_storm[files_h].shelter_edge[rank] = aa_shelter[rank];
    shelter_storm[files_h].storm_edge[rank] = aa_storm[rank];
    shelter_storm[files_h].shelter_middle[rank] = ab_shelter[rank];
    shelter_storm[files_h].storm_middle[rank] = ab_storm[rank];
    shelter_storm[files_h].shelter_center[rank] = ac_shelter[rank];
    shelter_storm[files_h].storm_center[rank] = ac_storm[rank];
    shelter_storm[files_a].shelter_diag[rank] = shelter_diag_a[rank];
    shelter_storm[files_h].shelter_diag[rank] = shelter_diag_a[rank];
  }

  for (rank = rank_1; rank <= rank_8; rank++) {
    shelter_storm[files_b].shelter_edge[rank] = ba_shelter[rank];
    shelter_storm[files_b].storm_edge[rank] = ba_storm[rank];
    shelter_storm[files_b].shelter_middle[rank] = bb_shelter[rank];
    shelter_storm[files_b].storm_middle[rank] = bb_storm[rank];
    shelter_storm[files_b].shelter_center[rank] = bc_shelter[rank];
    shelter_storm[files_b].storm_center[rank] = bc_storm[rank];
    shelter_storm[files_g].shelter_edge[rank] = ba_shelter[rank];
    shelter_storm[files_g].storm_edge[rank] = ba_storm[rank];
    shelter_storm[files_g].shelter_middle[rank] = bb_shelter[rank];
    shelter_storm[files_g].storm_middle[rank] = bb_storm[rank];
    shelter_storm[files_g].shelter_center[rank] = bc_shelter[rank];
    shelter_storm[files_g].storm_center[rank] = bc_storm[rank];
    shelter_storm[files_b].shelter_diag[rank] = shelter_diag_b[rank];
    shelter_storm[files_g].shelter_diag[rank] = shelter_diag_b[rank];
  }

  for (rank = rank_1; rank <= rank_8; rank++) {
    shelter_storm[files_c].shelter_edge[rank] = cb_shelter[rank];
    shelter_storm[files_c].storm_edge[rank] = cb_storm[rank];
    shelter_storm[files_c].shelter_middle[rank] = cc_shelter[rank];
    shelter_storm[files_c].storm_middle[rank] = cc_storm[rank];
    shelter_storm[files_c].shelter_center[rank] = cd_shelter[rank];
    shelter_storm[files_c].storm_center[rank] = cd_storm[rank];
    shelter_storm[files_f].shelter_edge[rank] = cb_shelter[rank];
    shelter_storm[files_f].storm_edge[rank] = cb_storm[rank];
    shelter_storm[files_f].shelter_middle[rank] = cc_shelter[rank];
    shelter_storm[files_f].storm_middle[rank] = cc_storm[rank];
    shelter_storm[files_f].shelter_center[rank] = cd_shelter[rank];
    shelter_storm[files_f].storm_center[rank] = cd_storm[rank];
    shelter_storm[files_c].shelter_diag[rank] = shelter_diag_c[rank];
    shelter_storm[files_f].shelter_diag[rank] = shelter_diag_c[rank];
  }

  for (rank = rank_1; rank <= rank_8; rank++) {
    shelter_storm[files_d].shelter_edge[rank] = dc_shelter[rank];
    shelter_storm[files_d].storm_edge[rank] = dc_storm[rank];
    shelter_storm[files_d].shelter_middle[rank] = dd_shelter[rank];
    shelter_storm[files_d].storm_middle[rank] = dd_storm[rank];
    shelter_storm[files_d].shelter_center[rank] = de_shelter[rank];
    shelter_storm[files_d].storm_center[rank] = de_storm[rank];
    shelter_storm[files_e].shelter_edge[rank] = dc_shelter[rank];
    shelter_storm[files_e].storm_edge[rank] = dc_storm[rank];
    shelter_storm[files_e].shelter_middle[rank] = dd_shelter[rank];
    shelter_storm[files_e].storm_middle[rank] = dd_storm[rank];
    shelter_storm[files_e].shelter_center[rank] = de_shelter[rank];
    shelter_storm[files_e].storm_center[rank] = de_storm[rank];
    shelter_storm[files_d].shelter_diag[rank] = shelter_diag_d[rank];
    shelter_storm[files_e].shelter_diag[rank] = shelter_diag_d[rank];
  }

  for (file = files_a; file <= files_h; file++) {
    shelter_storm[file].score_is_zero = shelter_storm[file].shelter_edge[rank_2]
      + shelter_storm[file].shelter_middle[rank_2]
      + shelter_storm[file].shelter_center[rank_2];
    shelter_storm[file].set_score_zero = 10;
  }
}

int white_king_danger(const int white_king_square) {
  const int
    rank = white_king_square >> 3;
  const uint64_t A = board.piece[white_pawn] & not_in_front_black[rank];
  const auto [edge, middle, center, shelter_edge, shelter_middle,
    shelter_center, storm_edge, storm_middle, storm_center,
    shelter_diag, score_is_zero, set_score_zero] = shelter_storm[(white_king_square & 7)];
  uint64_t T = A & edge;
  int horizontal_white_a = bsf(T);

  if (!T)
    horizontal_white_a = 0;
  horizontal_white_a >>= 3;
  T = A & middle;
  int horizontal_white_b = bsf(T);

  if (!T)
    horizontal_white_b = 0;
  horizontal_white_b >>= 3;
  T = A & center;
  int horizontal_white_c = bsf(T);

  if (!T)
    horizontal_white_c = 0;
  horizontal_white_c >>= 3;
  T = board.piece[black_pawn] & edge;
  int horizontal_black_a = bsf(T);

  if (!T)
    horizontal_black_a = 0;
  horizontal_black_a >>= 3;
  T = board.piece[black_pawn] & middle;
  int horizontal_black_b = bsf(T);

  if (!T)
    horizontal_black_b = 0;
  horizontal_black_b >>= 3;
  T = board.piece[black_pawn] & center;
  int horizontal_black_c = bsf(T);

  if (!T)
    horizontal_black_c = 0;
  horizontal_black_c >>= 3;
  int value = shelter_edge[horizontal_white_a] + shelter_middle[horizontal_white_b]
    + shelter_center[horizontal_white_c];

  if (value == score_is_zero)
    value = set_score_zero;
  T = A & diagonal_length[white_king_square];
  int e = bsf(T);

  if (!T)
    e = 0;
  e >>= 3;
  value += shelter_diag[e];
  value += storm_edge[horizontal_black_a] + storm_middle[horizontal_black_b] + storm_center[
    horizontal_black_c];
  return value;
}

int black_king_danger(const int black_king_square) {
  const int
    rank = black_king_square >> 3;
  const uint64_t A = board.piece[black_pawn] & not_in_front_white[rank];
  const auto [edge, middle, center, shelter_edge, shelter_middle,
    shelter_center, storm_edge, storm_middle, storm_center,
    shelter_diag, score_is_zero, set_score_zero] = shelter_storm[(black_king_square & 7)];
  uint64_t T = A & edge;
  int horizontal_black_a = bsr(T);

  if (!T)
    horizontal_black_a = 56;
  horizontal_black_a >>= 3;
  horizontal_black_a = 7 - horizontal_black_a;
  T = A & middle;
  int horizontal_black_b = bsr(T);

  if (!T)
    horizontal_black_b = 56;
  horizontal_black_b >>= 3;
  horizontal_black_b = 7 - horizontal_black_b;
  T = A & center;
  int horizontal_black_c = bsr(T);

  if (!T)
    horizontal_black_c = 56;
  horizontal_black_c >>= 3;
  horizontal_black_c = 7 - horizontal_black_c;
  T = board.piece[white_pawn] & edge;
  int horizontal_white_a = bsr(T);

  if (!T)
    horizontal_white_a = 56;
  horizontal_white_a >>= 3;
  horizontal_white_a = 7 - horizontal_white_a;
  T = board.piece[white_pawn] & middle;
  int horizontal_white_b = bsr(T);

  if (!T)
    horizontal_white_b = 56;
  horizontal_white_b >>= 3;
  horizontal_white_b = 7 - horizontal_white_b;
  T = board.piece[white_pawn] & center;
  int horizontal_white_c = bsr(T);

  if (!T)
    horizontal_white_c = 56;
  horizontal_white_c >>= 3;
  horizontal_white_c = 7 - horizontal_white_c;
  int value = shelter_edge[horizontal_black_a] + shelter_middle[horizontal_black_b]
    + shelter_center[horizontal_black_c];

  if (value == score_is_zero)
    value = set_score_zero;
  T = A & diagonal_length[black_king_square];
  int e = bsr(T);

  if (!T)
    e = 56;
  e >>= 3;
  e = 7 - e;
  value += shelter_diag[e];
  value += storm_edge[horizontal_white_a] + storm_middle[horizontal_white_b] + storm_center[
    horizontal_white_c];
  return value;
}

void eval_pawns(type_pawn_hash* result) {
  int c, score = 0, B, white_king_distance, black_king_distance, white_king_distance_best, black_king_distance_best;
  int white_king_square = board.white_king, black_king_square = board.black_king;
  int b, rank, file, value, passed_pawn_score;
  uint64_t T, U, V, connected;
  result->white_pawn_white = result->black_pawn_white = result->white_pawn_black = result->black_pawn_black = 0;
  result->white_king_danger = result->black_king_danger = 0;
  result->white_passed_pawn_file = result->black_passed_pawn_file = 0;
  black_king_distance_best = white_king_distance_best = 10000;
  connected = 0;
  c = 0;

  for (file = files_a; file <= files_h; file++) {
    if ((board.piece[white_pawn] & file_table[file]) == 0)
      c = 0;
    else {
      if (c == 0)
        score -= (0 << 16) + 3;
      c = 1;
    }
  }
  T = board.piece[white_pawn];

  while (T) {
    b = bsf(T);
    T &= T - 1;
    rank = b >> 3;
    file = b & 7;

    white_king_distance = DISTANCE_KING_PAWN_WHITE(b, white_king_square);

    if (white_king_distance < white_king_distance_best)
      white_king_distance_best = white_king_distance;

    black_king_distance = DISTANCE_KING_PAWN_WHITE(b, black_king_square);

    if (black_king_distance < black_king_distance_best)
      black_king_distance_best = black_king_distance;

    if (square_set[b] & 0x55aa55aa55aa55aa) {
      result->white_pawn_white += blocked_pawn[b];

      if (board.square[b + 8] == black_pawn)
        result->white_pawn_white += blocked_pawn[b];
    }
    else {
      result->white_pawn_black += blocked_pawn[b];

      if (board.square[b + 8] == black_pawn)
        result->white_pawn_black += blocked_pawn[b];
    }

    if (board.piece[white_pawn] & west_two[b]
      && (board.piece[white_pawn] & in_front_white[rank - 1]
        & file_table[file - 1]) == 0)
      score -= (1 << 16) + 2;

    if ((board.piece[white_pawn]
      | board.piece[black_pawn]) & open_file_white[b]) {
      if (board.piece[white_pawn] & table_gain[b]) {
        score -= (2 << 16) + 4;

        if ((board.piece[white_pawn] & files_isolated[file]) == 0)
          score -= (2 << 16) + 4;
      }

      if ((board.piece[white_pawn] & files_isolated[file]) == 0) {
        score -= (5 << 16) + 8;
        continue;
      }

      if ((board.piece[white_pawn] & pawn_protected_white[b]) == 0) {
        B = b + 8;

        if ((board.piece[white_pawn] & attack_pawn_black[b]) == 0) {
          B += 8;

          if ((board.piece[white_pawn] & attack_pawn_black[b + 8]) == 0)
            B += 8;
        }

        if (board.piece[black_pawn] & attack_pawn_black[B])
          score -= (5 << 16) + 5;
      }
      continue;
    }

    if (board.piece[white_pawn] & table_gain[b]) {
      score -= (4 << 16) + 8;

      if ((board.piece[white_pawn] & files_isolated[file]) == 0)
        score -= (6 << 16) + 10;
    }

    if ((board.piece[white_pawn] & files_isolated[file]) == 0)
      score -= (15 << 16) + 20;
    else {
      if ((board.piece[white_pawn] & pawn_protected_white[b]) == 0) {
        B = b + 8;

        if ((board.piece[white_pawn] & attack_pawn_black[b]) == 0) {
          B += 8;

          if ((board.piece[white_pawn] & attack_pawn_black[b + 8]) == 0)
            B += 8;
        }

        if (board.piece[black_pawn] & attack_pawn_black[B])
          score -= (10 << 16) + 15;
      }
    }

    if ((board.piece[black_pawn] & passed_pawn_white[b]) == 0)
      goto passed_pawns_white;

    if (board.piece[black_pawn] & passed_pawn_white[b] & ~attack_pawn_black[b]) {
      score += candidate_passed_pawn_value[rank];
      continue;
    }

    if (popcnt(attack_pawn_black[b] & board.piece[black_pawn])
      > popcnt(attack_pawn_white[b] & board.piece[white_pawn])) {
      score += candidate_passed_pawn_value[rank];
      continue;
    }
  passed_pawns_white:
    passed_pawn_score = passed_pawn_value[rank];

    if (board.piece[white_pawn] & attack_pawn_white[b])
      passed_pawn_score += protected_passed_pawn_value[rank];

    if ((board.piece[black_pawn] & files_left[file]) == 0
      || (board.piece[black_pawn] & files_right[file]) == 0)
      passed_pawn_score += outside_passed_pawn_value[rank];
    V = passed_pawn_connected[b] & connected;
    connected |= square_set[b];

    if (V) {
      passed_pawn_score +=
        connected_passed_pawn_value[rank] + connected_passed_pawn_value[(bsf(V) >> 3)];
      V &= V - 1;

      if (V)
        passed_pawn_score +=
          connected_passed_pawn_value[rank] + connected_passed_pawn_value[(bsf(V) >> 3)];
    }
    score += passed_pawn_score;
    result->white_passed_pawn_file |= static_cast<uint8_t>(1 << file);

    if (b <= h3)
      continue;
    score += DISTANCE_KING_PAWN_WHITE(b + 8, black_king_square) * opp_king_pawn_distance[RANK(b)];
    score -= DISTANCE_KING_PAWN_WHITE(b + 8, white_king_square) * my_king_pawn_distance[RANK(b)];
  }

  c = 0;

  for (file = files_a; file <= files_h; file++) {
    if ((board.piece[black_pawn] & file_table[file]) == 0)
      c = 0;
    else {
      if (c == 0)
        score += (0 << 16) + 3;
      c = 1;
    }
  }
  connected = 0;
  T = board.piece[black_pawn];

  while (T) {
    b = bsf(T);
    T &= T - 1;
    rank = b >> 3;
    file = b & 7;

    black_king_distance = DISTANCE_KING_PAWN_BLACK(b, black_king_square);

    if (black_king_distance < black_king_distance_best)
      black_king_distance_best = black_king_distance;

    white_king_distance = DISTANCE_KING_PAWN_BLACK(b, white_king_square);

    if (white_king_distance < white_king_distance_best)
      white_king_distance_best = white_king_distance;

    if (square_set[b] & 0x55aa55aa55aa55aa) {
      result->black_pawn_white += blocked_pawn[b];

      if (board.square[b - 8] == white_pawn)
        result->black_pawn_white += blocked_pawn[b];
    }
    else {
      result->black_pawn_black += blocked_pawn[b];

      if (board.square[b - 8] == white_pawn)
        result->black_pawn_black += blocked_pawn[b];
    }

    if (board.piece[black_pawn] & west_two[b]
      && (board.piece[black_pawn] & in_front_black[rank
        + 1] & file_table[file - 1]) == 0)
      score += (1 << 16) + 2;

    if ((board.piece[white_pawn]
      | board.piece[black_pawn]) & open_file_black[b]) {
      if (board.piece[black_pawn] & table_gain[b]) {
        score += (2 << 16) + 4;

        if ((board.piece[black_pawn] & files_isolated[file]) == 0)
          score += (2 << 16) + 4;
      }

      if ((board.piece[black_pawn] & files_isolated[file]) == 0) {
        score += (5 << 16) + 8;
        continue;
      }

      if ((board.piece[black_pawn] & pawn_protected_black[b]) == 0) {
        B = b - 8;

        if ((board.piece[black_pawn] & attack_pawn_white[b]) == 0) {
          B -= 8;

          if ((board.piece[black_pawn] & attack_pawn_white[b - 8]) == 0)
            B -= 8;
        }

        if (board.piece[white_pawn] & attack_pawn_white[B])
          score += (5 << 16) + 5;
      }
      continue;
    }

    if (board.piece[black_pawn] & table_gain[b]) {
      score += (4 << 16) + 8;

      if ((board.piece[black_pawn] & files_isolated[file]) == 0)
        score += (6 << 16) + 10;
    }

    if ((board.piece[black_pawn] & files_isolated[file]) == 0)
      score += (15 << 16) + 20;
    else {
      if ((board.piece[black_pawn] & pawn_protected_black[b]) == 0) {
        B = b - 8;

        if ((board.piece[black_pawn] & attack_pawn_white[b]) == 0) {
          B -= 8;

          if ((board.piece[black_pawn] & attack_pawn_white[b - 8]) == 0)
            B -= 8;
        }

        if (board.piece[white_pawn] & attack_pawn_white[B])
          score += (10 << 16) + 15;
      }
    }

    if ((board.piece[white_pawn] & passed_pawn_black[b]) == 0)
      goto passed_pawns_black;

    if (board.piece[white_pawn] & passed_pawn_black[b] & ~attack_pawn_white[b]) {
      score -= candidate_passed_pawn_value[7 - rank];
      continue;
    }

    if (popcnt(attack_pawn_white[b] & board.piece[white_pawn])
      > popcnt(attack_pawn_black[b] & board.piece[black_pawn])) {
      score -= candidate_passed_pawn_value[7 - rank];
      continue;
    }
  passed_pawns_black:
    passed_pawn_score = passed_pawn_value[7 - rank];

    if (board.piece[black_pawn] & attack_pawn_black[b])
      passed_pawn_score += protected_passed_pawn_value[7 - rank];

    if ((board.piece[white_pawn] & files_left[file]) == 0
      || (board.piece[white_pawn] & files_right[file]) == 0)
      passed_pawn_score += outside_passed_pawn_value[7 - rank];
    V = passed_pawn_connected[b] & connected;
    connected |= square_set[b];

    if (V) {
      passed_pawn_score +=
        connected_passed_pawn_value[7 - rank] + connected_passed_pawn_value[7 - (bsf(V) >> 3)];
      V &= V - 1;

      if (V)
        passed_pawn_score +=
          connected_passed_pawn_value[7 - rank] + connected_passed_pawn_value[7 - (bsf(V) >> 3)];
    }
    score -= passed_pawn_score;
    result->black_passed_pawn_file |= static_cast<uint8_t>(1 << file);

    if (b >= a6)
      continue;
    score -= DISTANCE_KING_PAWN_BLACK(b - 8, white_king_square) * opp_king_pawn_distance[rank_8 - RANK(b)];
    score += DISTANCE_KING_PAWN_BLACK(b - 8, black_king_square) * my_king_pawn_distance[rank_8 - RANK(b)];
  }

  T = 0;

  for (rank = rank_2; rank <= rank_7; rank++)
    T |= board.piece[white_pawn] >> 8 * rank & 0xff;
  U = 0;

  for (rank = rank_2; rank <= rank_7; rank++)
    U |= board.piece[black_pawn] >> 8 * rank & 0xff;
  result->white_draw_weight = opposing_pawns_multiplier[popcnt(T & ~U)] * pawn_count_multiplier[popcnt(T)];
  result->black_draw_weight = opposing_pawns_multiplier[popcnt(U & ~T)] * pawn_count_multiplier[popcnt(U)];

  if (board.piece[white_pawn] | board.piece[black_pawn])
    score += black_king_distance_best - white_king_distance_best;
  T = (board.piece[black_pawn] & ~0x0101010101010101) >> 9
    | (board.piece[black_pawn] & ~0x8080808080808080) >> 7;

  if (~T & attack_king[white_king_square] & board.piece[black_pawn])
    score += (0 << 16) + 5;

  if (position->castle & 1)
    score += (5 << 16) + 0;

  if (position->castle & 2)
    score += (5 << 16) + 0;
  T = (board.piece[white_pawn] & ~0x0101010101010101) << 7
    | (board.piece[white_pawn] & ~0x8080808080808080) << 9;

  if (~T & attack_king[black_king_square] & board.piece[white_pawn])
    score -= (0 << 16) + 5;

  if (position->castle & 4)
    score -= (5 << 16) + 0;

  if (position->castle & 8)
    score -= (5 << 16) + 0;
  result->pawn_hash_key = position->pawn_hash_key;
  result->score = score;
  value = white_king_danger(white_king_square);

  if (position->castle & 0x1)
    value = value <= 5 + white_king_danger(g1) ? value : 5 + white_king_danger(g1);

  if (position->castle & 0x2)
    value = value <= 5 + white_king_danger(c1) ? value : 5 + white_king_danger(c1);
  result->white_king_danger = (value << 16) + 0;
  value = black_king_danger(black_king_square);

  if (position->castle & 0x4)
    value = value <= 5 + black_king_danger(g8) ? value : 5 + black_king_danger(g8);

  if (position->castle & 0x8)
    value = value <= 5 + black_king_danger(c8) ? value : 5 + black_king_danger(c8);
  result->black_king_danger = (value << 16) + 0;
}

uint8_t black_see(const uint32_t move) {
  uint64_t sp[4], kr = 0;
  int index_turn[4];
  uint64_t T = position->black_xray & board.piece[occupied_white];
  int from = move >> 6 & 077;
  const int to = move & 077;

  while (T) {
    const int b = bsf(T);
    const int w = xray_table[b];
    T &= T - 1;

    if (from != w && direction[to][b] != direction[b][board.white_king])
      kr |= square_set[b];
  }
  kr = ~kr;
  int value_piece = see_value[board.square[from]];
  int value_capture = see_value[board.square[to]];

  if (value_piece - value_capture > value_pawn
    && attack_pawn_white[to] & board.piece[white_pawn] & kr)
    return 0;
  uint64_t attack_mask = (board.piece[black_knight]
    | (board.piece[white_knight] & kr)) & attack_knight[to];
  const int d = value_piece - value_capture;

  if (d > value_knight && board.piece[white_knight] & attack_mask)
    return 0;
  index_turn[0] = board.occupied_45_left >> line_turn[0][to] & 077;
  index_turn[1] = board.occupied_45_right >> line_turn[1][to] & 077;
  uint64_t mask = board.piece[black_queen] | (board.piece[black_queen_bishop] | board.piece[black_king_bishop])
    | ((board.piece[white_queen] | (board.piece[white_king_bishop] | board.piece[white_queen_bishop])) & kr);
  sp[0] = sp[1] = mask;
  attack_mask |= (bitboard_line_obscured[0][to][index_turn[0]] | bitboard_line_obscured[1][to][index_turn[1]]) & mask;

  if (d > value_bishop && (board.piece[white_king_bishop] | board.piece[white_queen_bishop]) & attack_mask)
    return 0;
  index_turn[2] = board.occupied_total >> line_turn[2][to] & 077;
  index_turn[3] = board.occupied_90_left >> line_turn[3][to] & 077;
  mask = board.piece[black_queen] | board.piece[black_rook]
    | ((board.piece[white_queen] | board.piece[white_rook]) & kr);
  sp[2] = sp[3] = mask;
  attack_mask |= (bitboard_line_obscured[2][to][index_turn[2]] | bitboard_line_obscured[3][to][index_turn[3]]) & mask;
  attack_mask |= (board.piece[black_king]
    | board.piece[white_king]) & attack_king[to];
  attack_mask |= board.piece[white_pawn] & attack_pawn_white[to] & kr;
  attack_mask |= board.piece[black_pawn] & attack_pawn_black[to];
  uint64_t attack_mask_not = ~(square_set[from] | square_set[to]);
  attack_mask &= attack_mask_not;
  int dir = direction[from][to];

  if (dir != 37)
    attack_mask |= bitboard_line_obscured[dir][from][index_turn[dir]] & sp[dir] & attack_mask_not;
  value_capture -= value_piece;

  do {
    attack_mask_not &= ~attack_mask;
    mask = board.piece[white_pawn] & attack_mask;

    if (mask) {
      attack_mask ^= ~(mask - 1) & mask;
      value_piece = value_pawn;
    }
    else {
      mask = board.piece[white_knight] & attack_mask;

      if (mask) {
        attack_mask ^= ~(mask - 1) & mask;
        value_piece = value_knight;
      }
      else {
        mask = (board.piece[white_king_bishop] | board.piece[white_queen_bishop]) & attack_mask;

        if (mask) {
          value_piece = value_bishop;
          from = bsf(mask);
          dir = direction[from][to];
          mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[1];
          attack_mask = mask | (square_clear[from] & attack_mask);
        }
        else {
          mask = board.piece[white_rook] & attack_mask;

          if (mask) {
            value_piece = value_rook;
            from = bsf(mask);
            dir = direction[from][to];
            mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[2];
            attack_mask = mask | (square_clear[from] & attack_mask);
          }
          else {
            mask = board.piece[white_queen] & attack_mask;

            if (mask) {
              value_piece = value_queen;
              from = bsf(mask);
              dir = direction[from][to];
              mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[dir];
              attack_mask = mask | (square_clear[from] & attack_mask);
            }
            else {
              if (!(board.piece[white_king] & attack_mask))
                return 1;
              value_piece = 12345;
            }
          }
        }
      }
    }
    value_capture += value_piece;

    if (value_capture < -60)
      return 0;
    mask = board.piece[black_pawn] & attack_mask;

    if (mask) {
      attack_mask ^= ~(mask - 1) & mask;
      value_piece = value_pawn;
    }
    else {
      mask = board.piece[black_knight] & attack_mask;

      if (mask) {
        attack_mask ^= ~(mask - 1) & mask;
        value_piece = value_knight;
      }
      else {
        mask = (board.piece[black_queen_bishop] | board.piece[black_king_bishop]) & attack_mask;

        if (mask) {
          value_piece = value_bishop;
          from = bsf(mask);
          dir = direction[from][to];
          mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[1];
          attack_mask = mask | (square_clear[from] & attack_mask);
        }
        else {
          mask = board.piece[black_rook] & attack_mask;

          if (mask) {
            value_piece = value_rook;
            from = bsf(mask);
            dir = direction[from][to];
            mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[2];
            attack_mask = mask | (square_clear[from] & attack_mask);
          }
          else {
            mask = board.piece[black_queen] & attack_mask;

            if (mask) {
              value_piece = value_queen;
              from = bsf(mask);
              dir = direction[from][to];
              mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[dir];
              attack_mask = mask | (square_clear[from] & attack_mask);
            }
            else {
              if (!(board.piece[black_king] & attack_mask))
                return 0;

              if (value_capture > 6174)
                return 1;
              value_piece = 23456;
            }
          }
        }
      }
    }
    value_capture -= value_piece;
  }
  while (value_capture < -60);
  return 1;
}

uint8_t white_see(const uint32_t move) {
  uint64_t sp[4], kr = 0;
  int index_turn[4];
  uint64_t T = position->white_xray & board.piece[occupied_black];
  int from = move >> 6 & 077;
  const int to = move & 077;

  while (T) {
    const int b = bsf(T);
    const int w = xray_table[b];
    T &= T - 1;

    if (from != w && direction[to][b] != direction[b][board.black_king])
      kr |= square_set[b];
  }
  kr = ~kr;
  int value_piece = see_value[board.square[from]];
  int value_capture = see_value[board.square[to]];

  if (value_piece - value_capture > value_pawn
    && attack_pawn_black[to] & board.piece[black_pawn] & kr)
    return 0;
  uint64_t attack_mask = (board.piece[white_knight]
    | (board.piece[black_knight] & kr)) & attack_knight[to];
  const int d = value_piece - value_capture;

  if (d > value_knight && board.piece[black_knight] & attack_mask)
    return 0;
  index_turn[0] = board.occupied_45_left >> line_turn[0][to] & 077;
  index_turn[1] = board.occupied_45_right >> line_turn[1][to] & 077;
  uint64_t mask = board.piece[white_queen] | (board.piece[white_king_bishop] | board.piece[white_queen_bishop])
    | ((board.piece[black_queen] | (board.piece[black_queen_bishop] | board.piece[black_king_bishop])) & kr);
  sp[0] = sp[1] = mask;
  attack_mask |= (bitboard_line_obscured[0][to][index_turn[0]] | bitboard_line_obscured[1][to][index_turn[1]]) & mask;

  if (d > value_bishop && (board.piece[black_queen_bishop] | board.piece[black_king_bishop]) & attack_mask)
    return 0;
  index_turn[2] = board.occupied_total >> line_turn[2][to] & 077;
  index_turn[3] = board.occupied_90_left >> line_turn[3][to] & 077;
  mask = board.piece[white_queen] | board.piece[white_rook]
    | ((board.piece[black_queen] | board.piece[black_rook]) & kr);
  sp[2] = sp[3] = mask;
  attack_mask |= (bitboard_line_obscured[2][to][index_turn[2]] | bitboard_line_obscured[3][to][index_turn[3]]) & mask;
  attack_mask |= (board.piece[white_king]
    | board.piece[black_king]) & attack_king[to];
  attack_mask |= board.piece[black_pawn] & attack_pawn_black[to] & kr;
  attack_mask |= board.piece[white_pawn] & attack_pawn_white[to];
  uint64_t attack_mask_not = ~(square_set[from] | square_set[to]);
  attack_mask &= attack_mask_not;
  int dir = direction[from][to];

  if (dir != 37)
    attack_mask |= bitboard_line_obscured[dir][from][index_turn[dir]] & sp[dir] & attack_mask_not;
  value_capture -= value_piece;

  do {
    attack_mask_not &= ~attack_mask;
    mask = board.piece[black_pawn] & attack_mask;

    if (mask) {
      attack_mask ^= ~(mask - 1) & mask;
      value_piece = value_pawn;
    }
    else {
      mask = board.piece[black_knight] & attack_mask;

      if (mask) {
        attack_mask ^= ~(mask - 1) & mask;
        value_piece = value_knight;
      }
      else {
        mask = (board.piece[black_queen_bishop] | board.piece[black_king_bishop]) & attack_mask;

        if (mask) {
          value_piece = value_bishop;
          from = bsf(mask);
          dir = direction[from][to];
          mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[1];
          attack_mask = mask | (square_clear[from] & attack_mask);
        }
        else {
          mask = board.piece[black_rook] & attack_mask;

          if (mask) {
            value_piece = value_rook;
            from = bsf(mask);
            dir = direction[from][to];
            mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[2];
            attack_mask = mask | (square_clear[from] & attack_mask);
          }
          else {
            mask = board.piece[black_queen] & attack_mask;

            if (mask) {
              value_piece = value_queen;
              from = bsf(mask);
              dir = direction[from][to];
              mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[dir];
              attack_mask = mask | (square_clear[from] & attack_mask);
            }
            else {
              if (!(board.piece[black_king] & attack_mask))
                return 1;
              value_piece = 12345;
            }
          }
        }
      }
    }
    value_capture += value_piece;

    if (value_capture < -60)
      return 0;
    mask = board.piece[white_pawn] & attack_mask;

    if (mask) {
      attack_mask ^= ~(mask - 1) & mask;
      value_piece = value_pawn;
    }
    else {
      mask = board.piece[white_knight] & attack_mask;

      if (mask) {
        attack_mask ^= ~(mask - 1) & mask;
        value_piece = value_knight;
      }
      else {
        mask = (board.piece[white_king_bishop] | board.piece[white_queen_bishop]) & attack_mask;

        if (mask) {
          value_piece = value_bishop;
          from = bsf(mask);
          dir = direction[from][to];
          mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[1];
          attack_mask = mask | (square_clear[from] & attack_mask);
        }
        else {
          mask = board.piece[white_rook] & attack_mask;

          if (mask) {
            value_piece = value_rook;
            from = bsf(mask);
            dir = direction[from][to];
            mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[2];
            attack_mask = mask | (square_clear[from] & attack_mask);
          }
          else {
            mask = board.piece[white_queen] & attack_mask;

            if (mask) {
              value_piece = value_queen;
              from = bsf(mask);
              dir = direction[from][to];
              mask = bitboard_line_obscured[dir][from][index_turn[dir]] & attack_mask_not & sp[dir];
              attack_mask = mask | (square_clear[from] & attack_mask);
            }
            else {
              if (!(board.piece[white_king] & attack_mask))
                return 0;

              if (value_capture > 6174)
                return 1;
              value_piece = 23456;
            }
          }
        }
      }
    }
    value_capture -= value_piece;
  }
  while (value_capture < -60);
  return 1;
}

uint8_t black_ok(const uint32_t move) {
  const int to = move & 077;
  const uint64_t mask = square_set[to];
  const int from = move >> 6 & 077;
  const int piece = board.square[from];

  if (piece == 0)
    return 0;

  if (piece <= 7)
    return 0;
  const int capture = board.square[to];

  if (capture >= 8)
    return 0;

  if (capture == white_king)
    return 0;

  if (piece == black_pawn) {
    if (to <= h1 && (move & 070000) < 040000)
      return 0;

    if ((move & 070000) == 030000 && to == position->en_passant && (from == to + 7 || from == to + 9))
      return 1;

    if (from == to + 7 || from == to + 9) {
      if (square_set[to] & board.piece[occupied_white])
        return 1;
      return 0;
    }

    if (from == to + 8) {
      if ((square_set[to] & board.occupied_total) == 0)
        return 1;
      return 0;
    }

    if (from != to + 16 || from >> 3 != rank_7)
      return 0;

    if (board.occupied_total & square_set[(from - 8)])
      return 0;
    return 1;
  }

  if (piece == black_knight) {
    if (attack_knight[from] & mask)
      return 1;
    return 0;
  }

  if (piece == black_queen_bishop || piece == black_king_bishop) {
    if ((bitboard_line_obscured[1][from][board.occupied_45_right >> line_turn[1][from] & 077]
      | bitboard_line_obscured[0][from][board.occupied_45_left >> line_turn[0][from] & 077]) & mask)
      return 1;
    return 0;
  }

  if ((move & 070000) == 010000) {
    if (to == g8) {
      if (!(position->castle & 0x4) || board.occupied_total & 0x6000000000000000
        || position->white_attack & 0x6000000000000000)
        return 0;
      return 1;
    }

    if (to == c8) {
      if (!(position->castle & 0x8) || board.occupied_total & 0x0e00000000000000
        || position->white_attack & 0x0c00000000000000)
        return 0;
      return 1;
    }
  }

  if (piece == black_rook) {
    if ((bitboard_line_obscured[2][from][board.occupied_total >> line_turn[2][from] & 077]
      | bitboard_line_obscured[3][from][board.occupied_90_left >> line_turn[3][from] & 077]) & mask)
      return 1;
    return 0;
  }

  if (piece == black_queen) {
    if ((bitboard_line_obscured[2][from][board.occupied_total >> line_turn[2][from] & 077]
      | bitboard_line_obscured[3][from][board.occupied_90_left >> line_turn[3][from] & 077]
      | (bitboard_line_obscured[1][from][board.occupied_45_right >> line_turn[1][from] & 077]
        | bitboard_line_obscured[0][from][board.occupied_45_left >> line_turn[0][from] & 077])) & mask)
      return 1;
    return 0;
  }

  if (piece == black_king) {
    if (attack_king[from] & mask && (square_set[to] & position->white_attack) == 0)
      return 1;
    return 0;
  }
  return 0;
}

uint8_t white_ok(const uint32_t move) {
  const int to = move & 077;
  const uint64_t mask = square_set[to];
  const int from = move >> 6 & 077;
  const int piece = board.square[from];

  if (piece == 0)
    return 0;

  if (piece >= 8)
    return 0;
  const int capture = board.square[to];

  if (capture && capture <= 7)
    return 0;

  if (capture == black_king)
    return 0;

  if (piece == white_pawn) {
    if (to >= a8 && (move & 070000) < 040000)
      return 0;

    if ((move & 070000) == 030000 && to == position->en_passant && (from == to - 9 || from == to - 7))
      return 1;

    if (from == to - 9 || from == to - 7) {
      if (square_set[to] & board.piece[occupied_black])
        return 1;
      return 0;
    }

    if (from == to - 8) {
      if ((square_set[to] & board.occupied_total) == 0)
        return 1;
      return 0;
    }

    if (from != to - 16 || from >> 3 != rank_2)
      return 0;

    if (board.occupied_total & square_set[(from + 8)])
      return 0;
    return 1;
  }

  if (piece == white_knight) {
    if (attack_knight[from] & mask)
      return 1;
    return 0;
  }

  if (piece == white_king_bishop || piece == white_queen_bishop) {
    if ((bitboard_line_obscured[1][from][board.occupied_45_right >> line_turn[1][from] & 077]
      | bitboard_line_obscured[0][from][board.occupied_45_left >> line_turn[0][from] & 077]) & mask)
      return 1;
    return 0;
  }

  if ((move & 070000) == 010000) {
    if (to == g1) {
      if (!(position->castle & 0x1) || board.occupied_total & 0x0000000000000060
        || position->black_attack & 0x0000000000000060)
        return 0;
      return 1;
    }

    if (to == c1) {
      if (!(position->castle & 0x2) || board.occupied_total & 0x000000000000000e
        || position->black_attack & 0x000000000000000c)
        return 0;
      return 1;
    }
  }

  if (piece == white_rook) {
    if ((bitboard_line_obscured[2][from][board.occupied_total >> line_turn[2][from] & 077]
      | bitboard_line_obscured[3][from][board.occupied_90_left >> line_turn[3][from] & 077]) & mask)
      return 1;
    return 0;
  }

  if (piece == white_queen) {
    if ((bitboard_line_obscured[2][from][board.occupied_total >> line_turn[2][from] & 077]
      | bitboard_line_obscured[3][from][board.occupied_90_left >> line_turn[3][from] & 077]
      | (bitboard_line_obscured[1][from][board.occupied_45_right >> line_turn[1][from] & 077]
        | bitboard_line_obscured[0][from][board.occupied_45_left >> line_turn[0][from] & 077])) & mask)
      return 1;
    return 0;
  }

  if (piece == white_king) {
    if (attack_king[from] & mask && (square_set[to] & position->black_attack) == 0)
      return 1;
    return 0;
  }
  return 0;
}

void sort(const type_move_list* a1, type_move_list* a2, const uint32_t s1, const uint32_t s2, const uint32_t s3) {
  type_move_list *p, *q;

  if (a1 == a2)
    return;

  for (p = a2 - 1; p >= a1; p--) {
    if ((p->move & 0x7fff) != s1 && (p->move & 0x7fff) != s2 && (p->move & 0x7fff) != s3)
      break;
    p->move = 0;
  }

  while (p > a1) {
    p--;

    if (const int move = p->move; (move & 0x7fff) != s1 && (move & 0x7fff) != s2 && (move & 0x7fff) != s3) {
      for (q = p + 1; q < a2; q++) {
        if (move < q->move)
          (q - 1)->move = q->move;
        else
          break;
      }
      q--;
      q->move = move;
    }
    else {
      a2--;

      for (q = p; q < a2; q++)
        q->move = (q + 1)->move;
      a2->move = 0;
    }
  }
}

type_move_list* evasion(type_move_list* move_list, const uint64_t mask) {
  if (board.white_to_move)
    return white_evasion(move_list, mask);
  return black_evasion(move_list, mask);
}

type_move_list* ordinary(type_move_list* move_list) {
  if (board.white_to_move)
    return white_ordinary(move_list);
  return black_ordinary(move_list);
}

type_move_list* capture(type_move_list* move_list, const uint64_t mask) {
  if (board.white_to_move)
    return white_capture(move_list, mask & board.piece[occupied_black]);
  return black_capture(move_list, mask & board.piece[occupied_white]);
}

type_move_list* black_evasion(type_move_list* list, uint64_t mask2) {
  uint64_t U;
  int to, from, c;
  const int king_square = board.black_king;
  uint64_t attacks = position->black_king_check;
  int square = bsf(attacks);
  int piece = board.square[square];
  uint64_t mask = ~position->white_attack & ((piece == white_pawn ? attack_king[king_square] : 0)
    | evasion_table[king_square][square]) & ~board.piece[occupied_black] & mask2;
  attacks &= attacks - 1;

  if (attacks) {
    square = bsf(attacks);
    piece = board.square[square];
    mask = mask & ((piece == white_pawn ? 0xffffffffffffffff : 0) | evasion_table[king_square][square]);
    square = king_square;

    while (mask) {
      to = bsf(mask);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[black_king][c];
      mask &= mask - 1;
    }
    list->move = 0;
    return list;
  }
  mask2 &= interposition_table[king_square][square];
  square = king_square;

  while (mask) {
    to = bsf(mask);
    c = board.square[to];
    list++->move = square << 6 | to | capture_value[black_king][c];
    mask &= mask - 1;
  }

  if (!mask2) {
    list->move = 0;
    return list;
  }

  if ((board.piece[black_pawn] & ~0x8080808080808080)
    >> 7 & (mask2 & board.piece[occupied_white])) {
    to = bsf(mask2 & board.piece[occupied_white]);
    c = board.square[to];

    if (to <= h1) {
      (list++)->move = (070000 | (((to) + 7) << 6) | to)
        | (0x20 << 24) + capture_value[black_pawn][c];
      (list++)->move = (040000 | (((to) + 7) << 6) | to) | (0);
      (list++)->move = (060000 | (((to) + 7) << 6) | to) | (0);
      (list++)->move = (050000 | (((to) + 7) << 6) | to) | (0);
    }
    else { (list++)->move = ((((to) + 7) << 6) | to) | (capture_value[black_pawn][c]); }
  }

  if (((board.piece[black_pawn] & ~0x0101010101010101)
    >> 9) & (mask2 & board.piece[occupied_white])) {
    to = bsf(mask2 & board.piece[occupied_white]);
    c = board.square[to];

    if (to <= h1) {
      (list++)->move = (070000 | (((to) + 9) << 6) | to)
        | (0x20 << 24) + capture_value[black_pawn][c];
      (list++)->move = (040000 | (((to) + 9) << 6) | to) | (0);
      (list++)->move = (060000 | (((to) + 9) << 6) | to) | (0);
      (list++)->move = (050000 | (((to) + 9) << 6) | to) | (0);
    }
    else { (list++)->move = ((((to) + 9) << 6) | to) | (capture_value[black_pawn][c]); }
  }
  to = position->en_passant;

  if (to) {
    if ((board.piece[black_pawn] & ~0x8080808080808080) >> 7 & square_set[to]
      && square_set[(to + 8)] & mask2)
      (list++)->move = (030000 | (((to) + 7) << 6) | to)
        | capture_value[black_pawn][white_pawn];

    if ((board.piece[black_pawn] & ~0x0101010101010101) >> 9 & square_set[to]
      && square_set[(to + 8)] & mask2)
      (list++)->move = (030000 | (((to) + 9) << 6) | to)
        | capture_value[black_pawn][white_pawn];
  }
  uint64_t T = board.piece[black_pawn] & (((mask2 & board.piece[occupied_white])
    ^ mask2) << 8);

  while (T) {
    from = bsf(T);
    T &= T - 1;

    if (from <= h2) {
      list++->move = 070000 | from << 6 | from - 8
        | capture_value[black_pawn][0];
      list++->move = 040000 | from << 6 | from - 8 | 0;
      list++->move = 060000 | from << 6 | from - 8 | 0;
      list++->move = 050000 | from << 6 | from - 8 | 0;
    }
    else { list++->move = from << 6 | from - 8 | capture_value[black_pawn][0]; }
  }
  T = board.piece[black_pawn] & (((mask2 & board.piece[occupied_white])
    ^ mask2) << 16 & 0x00ff000000000000 & ~board.occupied_total << 8);

  while (T) {
    from = bsf(T);
    T &= T - 1;
    list++->move = from << 6 | from - 16 | capture_value[black_pawn][0];
  }

  for (U = board.piece[black_knight]; U; U &= U - 1) {
    square = bsf(U);
    T = attack_knight[square] & mask2;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[black_knight][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[black_queen_bishop] | board.piece[black_king_bishop]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left
        >> line_turn[0][square] & 077]) & mask2;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[black_queen_bishop][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[black_rook]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left
        >> line_turn[3][square] & 077]) & mask2;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[black_rook][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[black_queen]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077]
      | (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
        | bitboard_line_obscured[0][square][board.occupied_45_left
          >> line_turn[0][square] & 077])) & mask2;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[black_queen][c];
      T &= T - 1;
    }
  }
  list->move = 0;
  return list;
}

type_move_list* black_gain(type_move_list* list, const int score) {
  const uint64_t empty_squares = ~board.occupied_total;
  uint64_t U, T;
  int to, square, value;
  type_move_list* q;
  const type_move_list* sm = list;

  for (U = (board.piece[black_pawn] & 0x00ffffffffffff0000) >> 8 & empty_squares;
       U; U &= U - 1) {
    to = bsf(U);

    if ((to & 070) == 050 && board.square[(to - 8)] == 0) {
      value = static_cast<int>(max_increase[black_pawn][((((to) + 8) << 6) | ((to) - 8)) & 07777]);

      if (value >= score)
        (list++)->move = ((((to) + 8) << 6) | ((to) - 8)) | (value << 16);
    }
    value = static_cast<int>(max_increase[black_pawn][((((to) + 8) << 6) | to) & 07777]);

    if (value >= score)
      (list++)->move = ((((to) + 8) << 6) | to) | (value << 16);
  }

  for (U = board.piece[black_knight]; U; U &= U - 1) {
    square = bsf(U);
    T = attack_knight[square] & empty_squares;

    while (T) {
      to = bsf(T);
      value = static_cast<int>(max_increase[black_knight][(square << 6 | to) & 07777]);

      if (value >= score)
        list++->move = square << 6 | to | value << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[black_queen_bishop]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left
        >> line_turn[0][square] & 077]) & empty_squares;

    while (T) {
      to = bsf(T);
      value = static_cast<int>(max_increase[black_queen_bishop][(square << 6 | to) & 07777]);

      if (value >= score)
        list++->move = square << 6 | to | value << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[black_king_bishop]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left
        >> line_turn[0][square] & 077]) & empty_squares;

    while (T) {
      to = bsf(T);
      value = static_cast<int>(max_increase[black_king_bishop][(square << 6 | to) & 07777]);

      if (value >= score)
        list++->move = square << 6 | to | value << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[black_rook]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left
        >> line_turn[3][square] & 077]) & empty_squares;

    while (T) {
      to = bsf(T);
      value = static_cast<int>(max_increase[black_rook][(square << 6 | to) & 07777]);

      if (value >= score)
        list++->move = square << 6 | to | value << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[black_queen]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077]
      | (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
        | bitboard_line_obscured[0][square][board.occupied_45_left
          >> line_turn[0][square] & 077])) & empty_squares;

    while (T) {
      to = bsf(T);
      value = static_cast<int>(max_increase[black_queen][(square << 6 | to) & 07777]);

      if (value >= score)
        list++->move = square << 6 | to | value << 16;
      T &= T - 1;
    }
  }
  square = board.black_king;
  T = attack_king[square] & empty_squares & ~position->white_attack;

  while (T) {
    to = bsf(T);
    value = static_cast<int>(max_increase[black_king][(square << 6 | to) & 07777]);

    if (value >= score)
      list++->move = square << 6 | to | value << 16;
    T &= T - 1;
  }
  list->move = 0;

  for (type_move_list* p = list - 1; p >= sm; p--) {
    const int move = p->move;

    for (q = p + 1; q < list; q++) {
      if (move < q->move)
        (q - 1)->move = q->move;
      else
        break;
    }
    q--;
    q->move = move;
  }
  return list;
}

type_move_list* black_capture(type_move_list* list, const uint64_t mask) {
  uint64_t U, orthogonal_attack, diagonal_attack;
  int square, c;
  int to = position->en_passant;

  if (to) {
    if ((board.piece[black_pawn] & ~0x0101010101010101) >> 9 & square_set[to]) {
      (list++)->move = (030000 | (((to) + 9) << 6) | to)
        | capture_value[black_pawn][white_pawn];
    }

    if ((board.piece[black_pawn] & ~0x8080808080808080) >> 7 & square_set[to]) {
      (list++)->move = (030000 | (((to) + 7) << 6) | to)
        | capture_value[black_pawn][white_pawn];
    }
  }
	
	uint64_t T = (board.piece[black_pawn] & ~0x0101010101010101) >> 9 & ~0x00000000000000ff & mask;

	if ((mask & position->black_attack) == 0)
		goto empty_target;

  while (T) {
    to = bsf(T);
    c = board.square[to];
    (list++)->move = ((((to) + 9) << 6) | to) | (capture_value[black_pawn][c]);
    T &= T - 1;
  }
  T = (board.piece[black_pawn] & ~0x8080808080808080) >> 7 & ~0x00000000000000ff & mask;

  while (T) {
    to = bsf(T);
    c = board.square[to];
    (list++)->move = ((((to) + 7) << 6) | to) | (capture_value[black_pawn][c]);
    T &= T - 1;
  }

  for (U = board.piece[black_knight]; U; U &= U - 1) {
    square = bsf(U);
    T = attack_knight[square] & mask;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[black_knight][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[black_queen_bishop] | board.piece[black_king_bishop];
       U; U &= U - 1) {
    square = bsf(U);
    diagonal_attack = bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left >> line_turn[0][square] & 077];
    T = diagonal_attack & mask;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[black_queen_bishop][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[black_rook]; U; U &= U - 1) {
    square = bsf(U);
    orthogonal_attack = bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077];
    T = orthogonal_attack & mask;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[black_rook][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[black_queen]; U; U &= U - 1) {
    square = bsf(U);
    orthogonal_attack =
      bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077];
    diagonal_attack =
      bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left >> line_turn[0][square] & 077];
    T = (diagonal_attack | orthogonal_attack) & mask;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[black_queen][c];
      T &= T - 1;
    }
  }
  square = bsf(board.piece[black_king]);
  T = attack_king[square] & mask;

  while (T) {
    to = bsf(T);
    c = board.square[to];
    list++->move = square << 6 | to | capture_value[black_king][c];
    T &= T - 1;
  }
empty_target:
  for (U = board.piece[black_pawn] & 0x000000000000ff00; U; U &= U - 1) {
    square = bsf(U);
    to = square - 8;

    if (board.square[to] == 0) {
      list++->move = 070000 | square << 6 | to | 0xd8 << 24;

      if (attack_knight[to] & board.piece[white_king]) { list++->move = 040000 | square << 6 | to | 0xc2 << 24; }
    }
    to = square - 9;

    if (square != a2 && square_set[to] & mask) {
      c = board.square[to];
      list++->move = 070000 | square << 6 | to
        | (0x28 << 24) + capture_value[black_pawn][c];

      if (attack_knight[to] & board.piece[white_king]) {
        list++->move = 040000 | square << 6 | to
          | (0x1a << 24) + capture_value[black_pawn][c];
      }
    }
    to = square - 7;

    if (square != h2 && square_set[to] & mask) {
      c = board.square[to];
      list++->move = 070000 | square << 6 | to
        | (0x28 << 24) + capture_value[black_pawn][c];

      if (attack_knight[to] & board.piece[white_king]) {
        list++->move = 040000 | square << 6 | to
          | (0x1a << 24) + capture_value[black_pawn][c];
      }
    }
  }
  list->move = 0;
  return list;
}

type_move_list* black_ordinary(type_move_list* list) {
  const uint64_t empty_squares = ~board.occupied_total;
  uint64_t U, T, L = 0, next = 0;
  int to, square;
  const int king_square_rerun = board.white_king;

  if (position->castle & 0x4
    && ((board.occupied_total | position->white_attack) & 0x6000000000000000) == 0) {
    list++->move = 010000 | e8 << 6 | g8 | (square_set[g8] & 0 ? 0x8000 : 0)
      | history_table[black_king][g8] << 16;
  }

  if (position->castle & 0x8 && (board.occupied_total & 0x0e00000000000000) == 0
    && (position->white_attack & 0x0c00000000000000) == 0) {
    list++->move = 010000 | e8 << 6 | c8 | (square_set[c8] & 0 ? 0x8000 : 0)
      | history_table[black_king][c8] << 16;
  }
  const uint64_t K = attack_pawn_black[king_square_rerun];

  if (board.piece[black_queen] | board.piece[black_rook])
    L = bitboard_line_obscured[2][king_square_rerun][board.occupied_total
        >> line_turn[2][king_square_rerun] & 077]
      | bitboard_line_obscured[3][king_square_rerun][board.occupied_90_left
        >> line_turn[3][king_square_rerun] & 077];

  if (board.piece[black_queen]
    | (board.piece[black_queen_bishop] | board.piece[black_king_bishop]))
    next = bitboard_line_obscured[1][king_square_rerun][board.occupied_45_right
        >> line_turn[1][king_square_rerun] & 077]
      | bitboard_line_obscured[0][king_square_rerun][board.occupied_45_left
        >> line_turn[0][king_square_rerun] & 077];

  for (U = (board.piece[black_pawn] & 0x00ffffffffffff0000) >> 8 & empty_squares;
       U; U &= U - 1) {
    to = bsf(U);

    if ((to & 070) == 050 && board.square[(to - 8)] == 0) {
      (list++)->move = ((((to) + 8) << 6) | ((to) - 8)) | ((square_set[((to) - 8)] & (K)) ? 0x8000 : 0)
        | history_table[black_pawn][(to - 8)] << 16;
    }
    (list++)->move = ((((to) + 8) << 6) | to) | ((square_set[to] & (K)) ? 0x8000 : 0)
      | history_table[black_pawn][to] << 16;
  }

  for (U = board.piece[black_queen]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077]
      | (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
        | bitboard_line_obscured[0][square][board.occupied_45_left
          >> line_turn[0][square] & 077])) & empty_squares;

    while (T) {
      to = bsf(T);
      list++->move = square << 6 | to | (square_set[to] & (L | next) ? 0x8000 : 0)
        | history_table[black_queen][to] << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[black_rook]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left
        >> line_turn[3][square] & 077]) & empty_squares;

    while (T) {
      to = bsf(T);
      list++->move = square << 6 | to | (square_set[to] & L ? 0x8000 : 0)
        | history_table[black_rook][to] << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[black_queen_bishop] | board.piece[black_king_bishop];
       U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left
        >> line_turn[0][square] & 077]) & empty_squares;

    while (T) {
      to = bsf(T);
      list++->move = square << 6 | to | (square_set[to] & next ? 0x8000 : 0)
        | history_table[(square_set[square] & 0xaa55aa55aa55aa55 ? black_king_bishop : black_queen_bishop)][
          to] << 16;
      T &= T - 1;
    }
  }
  square = bsf(board.piece[black_king]);
  T = attack_king[square] & empty_squares & ~position->white_attack;

  while (T) {
    to = bsf(T);
    list++->move = square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
      | history_table[black_king][to] << 16;
    T &= T - 1;
  }

  for (U = board.piece[black_knight]; U; U &= U - 1) {
    square = bsf(U);
    T = attack_knight[square] & empty_squares;

    while (T) {
      to = bsf(T);
      list++->move = square << 6 | to
        | (square_set[to] & attack_knight[king_square_rerun] ? 0x8000 : 0)
        | history_table[black_knight][to] << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[black_pawn] & 0x000000000000ff00; U; U &= U - 1) {
    square = bsf(U);
    to = square - 8;

    if (board.square[to] == 0) {
      if ((attack_knight[to] & board.piece[white_king]) == 0) {
        list++->move = 040000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
          | history_table[black_pawn][to] << 16;
      }
      list++->move = 060000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[black_pawn][to] << 16;
      list++->move = 050000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[black_pawn][to] << 16;
    }
    to = square - 9;

    if (square != a2 && square_set[to] & board.piece[occupied_white]) {
      if ((attack_knight[to] & board.piece[white_king]) == 0) {
        list++->move = 040000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
          | history_table[black_pawn][to] << 16;
      }
      list++->move = 060000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[black_pawn][to] << 16;
      list++->move = 050000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[black_pawn][to] << 16;
    }
    to = square - 7;

    if (square != h2 && square_set[to] & board.piece[occupied_white]) {
      if ((attack_knight[to] & board.piece[white_king]) == 0) {
        list++->move = 040000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
          | history_table[black_pawn][to] << 16;
      }
      list++->move = 060000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[black_pawn][to] << 16;
      list++->move = 050000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[black_pawn][to] << 16;
    }
  }
  list->move = 0;
  return list;
}

type_move_list* black_check(type_move_list* list, uint64_t mask) {
  int king_square_rerun, king_square, square, to, from, piece;
  uint64_t U, T, V;
  uint32_t move;
  uint64_t kr;
  kr = ~position->black_xray;
  mask = ~mask & ~board.piece[occupied_black];
  king_square = board.white_king;

  for (U = position->black_xray & board.piece[occupied_black]; U;
       U &= U - 1) {
    from = bsf(U);
    piece = board.square[from];

    if (piece == black_pawn) {
      if ((from & 7) != (king_square & 7) && from > h2 && board.square[(from - 8)] == 0) {
        list++->move = from << 6 | from - 8;

        if ((from & 070) == 060 && board.square[(from - 16)] == 0)
          list++->move = from << 6 | from - 16;
      }

      if ((square_set[from] & 0x7f7f7f7f7f7f7f7f)
        >> 7 & board.piece[occupied_white] & mask)
        list++->move = from << 6 | from - 7;

      if ((square_set[from] & 0xfefefefefefefefe)
        >> 9 & board.piece[occupied_white] & mask)
        list++->move = from << 6 | from - 9;
    }
    else if (piece == black_knight) {
      V = attack_knight[from] & mask;

      while (V) {
        to = bsf(V);
        list++->move = from << 6 | to;
        V &= V - 1;
      }
    }
    else if (piece == black_queen_bishop || piece == black_king_bishop) {
      V = (bitboard_line_obscured[1][from][board.occupied_45_right >> line_turn[1][from] & 077]
        | bitboard_line_obscured[0][from][board.occupied_45_left >> line_turn[0][from] & 077]) & mask;

      while (V) {
        to = bsf(V);
        list++->move = from << 6 | to;
        V &= V - 1;
      }
    }
    else if (piece == black_rook) {
      V = (bitboard_line_obscured[2][from][board.occupied_total >> line_turn[2][from] & 077]
        | bitboard_line_obscured[3][from][board.occupied_90_left >> line_turn[3][from] & 077]) & mask;

      while (V) {
        to = bsf(V);
        list++->move = from << 6 | to;
        V &= V - 1;
      }
    }
    else if (piece == black_king) {
      if ((from & 7) == (king_square & 7) || from >> 3 == king_square >> 3)
        V = attack_king[from] & non_orthogonal[king_square] & mask & ~position->white_attack;
      else
        V = attack_king[from] & non_diagonal[king_square] & mask & ~position->white_attack;

      while (V) {
        to = bsf(V);
        list++->move = from << 6 | to;
        V &= V - 1;
      }
    }
  }

  king_square_rerun = board.white_king;
  T = (board.piece[black_pawn] & ~0x0101010101010101)
    >> 9 & ~0x00000000000000ff & mask & board.
    piece[occupied_white] & attack_pawn_black[king_square_rerun];

  while (T) {
    to = bsf(T);
    (list++)->move = (((to) + 9) << 6) | to;
    T &= T - 1;
  }
  T = (board.piece[black_pawn] & ~0x8080808080808080)
    >> 7 & ~0x00000000000000ff & mask & board.
    piece[occupied_white] & attack_pawn_black[king_square_rerun];

  while (T) {
    to = bsf(T);
    (list++)->move = (((to) + 7) << 6) | to;
    T &= T - 1;
  }

  for (U = board.piece[black_queen]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077]
      | (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
        | bitboard_line_obscured[0][square][board.occupied_45_left
          >> line_turn[0][square] & 077])) & (bitboard_line_obscured[2][king_square][board.
        occupied_total >> line_turn[2][king_square] & 077]
      | bitboard_line_obscured[3][king_square][board.occupied_90_left
        >> line_turn[3][king_square] & 077]
      | (bitboard_line_obscured[1][king_square][board.occupied_45_right
          >> line_turn[1][king_square] & 077]
        | bitboard_line_obscured[0][king_square][board.occupied_45_left
          >> line_turn[0][king_square] & 077])) & mask;

    while (T) {
      to = bsf(T);
      T &= T - 1;

      if ((attack_pawn_white[to] & board.piece[white_pawn] & kr) == 0
        && (attack_knight[to] & board.piece[white_knight] & kr) == 0) {
        move = square << 6 | to;

        if (black_see(move))
          list++->move = square << 6 | to;
      }
    }
  }

  for (U = board.piece[black_rook]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left
        >> line_turn[3][square] & 077]) & (bitboard_line_obscured[2][king_square][board.
        occupied_total >> line_turn[2][king_square] & 077]
      | bitboard_line_obscured[3][king_square][board.occupied_90_left
        >> line_turn[3][king_square] & 077]) & mask;

    while (T) {
      to = bsf(T);
      T &= T - 1;

      if ((attack_pawn_white[to] & board.piece[white_pawn] & kr) == 0
        && (attack_knight[to] & board.piece[white_knight] & kr) == 0) {
        move = square << 6 | to;

        if (black_see(move))
          list++->move = square << 6 | to;
      }
    }
  }

  for (U = board.piece[black_queen_bishop] | board.piece[black_king_bishop];
       U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left
        >> line_turn[0][square] & 077]) & (bitboard_line_obscured[1][king_square][board.
        occupied_45_right >> line_turn[1][king_square] & 077]
      | bitboard_line_obscured[0][king_square][board.occupied_45_left
        >> line_turn[0][king_square] & 077]) & mask;

    while (T) {
      to = bsf(T);
      T &= T - 1;

      if ((attack_pawn_white[to] & board.piece[white_pawn] & kr) == 0) {
        move = square << 6 | to;

        if (black_see(move))
          list++->move = square << 6 | to;
      }
    }
  }

  for (U = board.piece[black_knight]; U; U &= U - 1) {
    square = bsf(U);
    T = attack_knight[square] & attack_knight[king_square] & mask;

    while (T) {
      to = bsf(T);
      T &= T - 1;

      if ((attack_pawn_white[to] & board.piece[white_pawn] & kr) == 0) {
        move = square << 6 | to;

        if (black_see(move))
          list++->move = square << 6 | to;
      }
    }
  }

  if (board.piece[white_king] & 0x0000007f7f7f7f7f
    && board.square[(king_square_rerun + 9)] == 0) {
    if (board.square[(king_square_rerun + 17)] == black_pawn) {
      from = king_square_rerun + 17;
      to = king_square_rerun + 9;
      move = from << 6 | to;

      if ((position->black_attack & square_set[to]
        || bitboard_line_obscured[3][from][board.occupied_90_left
          >> line_turn[3][from] & 077] & (board.piece[black_rook]
          | board.piece[black_queen])) && black_see(move))
        list++->move = move;
    }

    if (king_square_rerun >> 3 == rank_4 && board.square[(king_square_rerun + 17)] == 0
      && board.square[(king_square_rerun + 25)] == black_pawn) {
      to = king_square_rerun + 9;
      from = king_square_rerun + 25;
      move = from << 6 | to;

      if ((position->black_attack & square_set[to]
        || bitboard_line_obscured[3][from][board.occupied_90_left
          >> line_turn[3][from] & 077] & (board.piece[black_rook]
          | board.piece[black_queen])) && black_see(move))
        list++->move = move;
    }
  }

  if (board.piece[white_king] & 0x000000fefefefefe
    && board.square[(king_square_rerun + 7)] == 0) {
    if (board.square[(king_square_rerun + 15)] == black_pawn) {
      from = king_square_rerun + 15;
      to = king_square_rerun + 7;
      move = from << 6 | to;

      if ((position->black_attack & square_set[to]
        || bitboard_line_obscured[3][from][board.occupied_90_left
          >> line_turn[3][from] & 077] & (board.piece[black_rook]
          | board.piece[black_queen])) && black_see(move))
        list++->move = move;
    }

    if (king_square_rerun >> 3 == rank_4 && board.square[(king_square_rerun + 15)] == 0
      && board.square[(king_square_rerun + 23)] == black_pawn) {
      to = king_square_rerun + 7;
      from = king_square_rerun + 23;
      move = from << 6 | to;

      if ((position->black_attack & square_set[to]
        || bitboard_line_obscured[3][from][board.occupied_90_left
          >> line_turn[3][from] & 077] & (board.piece[black_rook]
          | board.piece[black_queen])) && black_see(move))
        list++->move = move;
    }
  }
  list->move = 0;
  return list;
}

type_move_list* white_evasion(type_move_list* list, uint64_t mask2) {
  uint64_t U;
  int to, from, c;
  const int king_square = board.white_king;
  uint64_t attacks = position->white_king_check;
  int square = bsf(attacks);
  int piece = board.square[square];
  uint64_t mask = ~position->black_attack & ((piece == black_pawn ? attack_king[king_square] : 0)
    | evasion_table[king_square][square]) & ~board.piece[occupied_white] & mask2;
  attacks &= attacks - 1;

  if (attacks) {
    square = bsf(attacks);
    piece = board.square[square];
    mask = mask & ((piece == black_pawn ? 0xffffffffffffffff : 0) | evasion_table[king_square][square]);
    square = king_square;

    while (mask) {
      to = bsf(mask);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[white_king][c];
      mask &= mask - 1;
    }
    list->move = 0;
    return list;
  }
  mask2 &= interposition_table[king_square][square];
  square = king_square;

  while (mask) {
    to = bsf(mask);
    c = board.square[to];
    list++->move = square << 6 | to | capture_value[white_king][c];
    mask &= mask - 1;
  }

  if (!mask2) {
    list->move = 0;
    return list;
  }

  if ((board.piece[white_pawn] & ~0x8080808080808080)
    << 9 & (mask2 & board.piece[occupied_black])) {
    to = bsf(mask2 & board.piece[occupied_black]);
    c = board.square[to];

    if (to >= a8) {
      (list++)->move = (070000 | (((to) - 9) << 6) | to)
        | (0x20 << 24) + capture_value[white_pawn][c];
      (list++)->move = (040000 | (((to) - 9) << 6) | to) | (0);
      (list++)->move = (060000 | (((to) - 9) << 6) | to) | (0);
      (list++)->move = (050000 | (((to) - 9) << 6) | to) | (0);
    }
    else { (list++)->move = ((((to) - 9) << 6) | to) | (capture_value[white_pawn][c]); }
  }

  if ((board.piece[white_pawn] & ~0x0101010101010101)
    << 7 & (mask2 & board.piece[occupied_black])) {
    to = bsf(mask2 & board.piece[occupied_black]);
    c = board.square[to];

    if (to >= a8) {
      (list++)->move = (070000 | (((to) - 7) << 6) | to)
        | (0x20 << 24) + capture_value[white_pawn][c];
      (list++)->move = (040000 | (((to) - 7) << 6) | to) | (0);
      (list++)->move = (060000 | (((to) - 7) << 6) | to) | (0);
      (list++)->move = (050000 | (((to) - 7) << 6) | to) | (0);
    }
    else { (list++)->move = ((((to) - 7) << 6) | to) | (capture_value[white_pawn][c]); }
  }
  to = position->en_passant;

  if (to) {
    if ((board.piece[white_pawn] & ~0x8080808080808080) << 9 & square_set[to]
      && square_set[(to - 8)] & mask2) {
      (list++)->move = (030000 | (((to) - 9) << 6) | to)
        | capture_value[white_pawn][black_pawn];
    }

    if ((board.piece[white_pawn] & ~0x0101010101010101) << 7 & square_set[to]
      && square_set[(to - 8)] & mask2) {
      (list++)->move = (030000 | (((to) - 7) << 6) | to)
        | capture_value[white_pawn][black_pawn];
    }
  }
  uint64_t T = board.piece[white_pawn] & (((mask2 & board.piece[occupied_black])
    ^ mask2) >> 8);

  while (T) {
    from = bsf(T);
    T &= T - 1;

    if (from >= a7) {
      list++->move = 070000 | from << 6 | from + 8
        | capture_value[white_pawn][0];
      list++->move = 040000 | from << 6 | from + 8 | 0;
      list++->move = 060000 | from << 6 | from + 8 | 0;
      list++->move = 050000 | from << 6 | from + 8 | 0;
    }
    else { list++->move = from << 6 | from + 8 | capture_value[white_pawn][0]; }
  }
  T = board.piece[white_pawn] & (((mask2 & board.piece[occupied_black])
    ^ mask2) >> 16 & 0x000000000000ff00 & ~board.occupied_total >> 8);

  while (T) {
    from = bsf(T);
    T &= T - 1;
    list++->move = from << 6 | from + 16 | capture_value[white_pawn][0];
  }

  for (U = board.piece[white_knight]; U; U &= U - 1) {
    square = bsf(U);
    T = attack_knight[square] & mask2;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[white_knight][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[white_king_bishop] | board.piece[white_queen_bishop]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left
        >> line_turn[0][square] & 077]) & mask2;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[white_king_bishop][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[white_rook]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left
        >> line_turn[3][square] & 077]) & mask2;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[white_rook][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[white_queen]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077]
      | (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
        | bitboard_line_obscured[0][square][board.occupied_45_left
          >> line_turn[0][square] & 077])) & mask2;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[white_queen][c];
      T &= T - 1;
    }
  }
  list->move = 0;
  return list;
}

type_move_list* white_gain(type_move_list* list, const int score) {
  const uint64_t empty_squares = ~board.occupied_total;
  uint64_t U, T;
  int to, square, value;
  type_move_list* q;
  const type_move_list* sm = list;

  for (U = (board.piece[white_pawn] & 0x0000ffffffffff00) << 8 & empty_squares; U;
       U &= U - 1) {
    to = bsf(U);

    if ((to & 070) == 020 && board.square[(to + 8)] == 0) {
      if (const int val = max_increase[white_pawn][((to - 8) << 6 | to + 8) & 07777]; val >= score)
        (list++)->move = ((((to) - 8) << 6) | ((to) + 8)) | (val << 16);
    }
    value = static_cast<int>(max_increase[white_pawn][((((to) - 8) << 6) | to) & 07777]);

    if (value >= score)
      (list++)->move = ((((to) - 8) << 6) | to) | (value << 16);
  }

  for (U = board.piece[white_knight]; U; U &= U - 1) {
    square = bsf(U);
    T = attack_knight[square] & empty_squares;

    while (T) {
      to = bsf(T);
      value = static_cast<int>(max_increase[white_knight][(square << 6 | to) & 07777]);

      if (value >= score)
        list++->move = square << 6 | to | value << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[white_king_bishop]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left
        >> line_turn[0][square] & 077]) & empty_squares;

    while (T) {
      to = bsf(T);
      value = static_cast<int>(max_increase[white_king_bishop][(square << 6 | to) & 07777]);

      if (value >= score)
        list++->move = square << 6 | to | value << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[white_queen_bishop]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
        | bitboard_line_obscured[0][square][board.occupied_45_left >> line_turn[0][square] & 077]) &
      empty_squares;

    while (T) {
      to = bsf(T);
      value = static_cast<int>(max_increase[white_queen_bishop][(square << 6 | to) & 07777]);

      if (value >= score)
        list++->move = square << 6 | to | value << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[white_rook]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left
        >> line_turn[3][square] & 077]) & empty_squares;

    while (T) {
      to = bsf(T);
      value = static_cast<int>(max_increase[white_rook][(square << 6 | to) & 07777]);

      if (value >= score)
        list++->move = square << 6 | to | value << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[white_queen]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077]
      | (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
        | bitboard_line_obscured[0][square][board.occupied_45_left
          >> line_turn[0][square] & 077])) & empty_squares;

    while (T) {
      to = bsf(T);
      value = static_cast<int>(max_increase[white_queen][(square << 6 | to) & 07777]);

      if (value >= score)
        list++->move = square << 6 | to | value << 16;
      T &= T - 1;
    }
  }
  square = board.white_king;
  T = attack_king[square] & empty_squares & ~position->black_attack;

  while (T) {
    to = bsf(T);
    value = static_cast<int>(max_increase[white_king][(square << 6 | to) & 07777]);

    if (value >= score)
      list++->move = square << 6 | to | value << 16;
    T &= T - 1;
  }
  list->move = 0;

  for (type_move_list* p = list - 1; p >= sm; p--) {
    const int move = p->move;

    for (q = p + 1; q < list; q++) {
      if (move < q->move)
        (q - 1)->move = q->move;
      else
        break;
    }
    q--;
    q->move = move;
  }
  return list;
}

type_move_list* white_capture(type_move_list* list, const uint64_t mask) {
  uint64_t U, orthogonal_attack, diagonal_attack;
  int square, c;
  int to = position->en_passant;

  if (to) {
    if ((board.piece[white_pawn] & ~0x0101010101010101) << 7 & square_set[to]) {
      (list++)->move = (030000 | (((to) - 7) << 6) | to)
        | capture_value[white_pawn][black_pawn];
    }

    if ((board.piece[white_pawn] & ~0x8080808080808080) << 9 & square_set[to]) {
      (list++)->move = (030000 | (((to) - 9) << 6) | to)
        | capture_value[white_pawn][black_pawn];
    }
  }

uint64_t T = (board.piece[white_pawn] & ~0x0101010101010101) << 7 & ~0xff00000000000000 & mask;

	if ((mask & position->white_attack) == 0)
		goto empty_target;

  while (T) {
    to = bsf(T);
    c = board.square[to];
    (list++)->move = ((((to) - 7) << 6) | to) | (capture_value[white_pawn][c]);
    T &= T - 1;
  }
  T = (board.piece[white_pawn] & ~0x8080808080808080) << 9 & ~0xff00000000000000 & mask;

  while (T) {
    to = bsf(T);
    c = board.square[to];
    (list++)->move = ((((to) - 9) << 6) | to) | (capture_value[white_pawn][c]);
    T &= T - 1;
  }

  for (U = board.piece[white_knight]; U; U &= U - 1) {
    square = bsf(U);
    T = attack_knight[square] & mask;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[white_knight][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[white_king_bishop] | board.piece[white_queen_bishop]; U; U &= U - 1) {
    square = bsf(U);
    diagonal_attack =
      bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left >> line_turn[0][square] & 077];
    T = diagonal_attack & mask;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[white_king_bishop][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[white_rook]; U; U &= U - 1) {
    square = bsf(U);
    orthogonal_attack =
      bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077];
    T = orthogonal_attack & mask;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[white_rook][c];
      T &= T - 1;
    }
  }

  for (U = board.piece[white_queen]; U; U &= U - 1) {
    square = bsf(U);
    orthogonal_attack = bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077];
    diagonal_attack = bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left >> line_turn[0][square] & 077];
    T = (diagonal_attack | orthogonal_attack) & mask;

    while (T) {
      to = bsf(T);
      c = board.square[to];
      list++->move = square << 6 | to | capture_value[white_queen][c];
      T &= T - 1;
    }
  }
  square = bsf(board.piece[white_king]);
  T = attack_king[square] & mask;

  while (T) {
    to = bsf(T);
    c = board.square[to];
    list++->move = square << 6 | to | capture_value[white_king][c];
    T &= T - 1;
  }
empty_target:
  for (U = board.piece[white_pawn] & 0x00ff000000000000; U; U &= U - 1) {
    square = bsf(U);
    to = square + 8;

    if (board.square[to] == 0) {
      list++->move = 070000 | square << 6 | to | 0xd8 << 24;

      if (attack_knight[to] & board.piece[black_king]) { list++->move = 040000 | square << 6 | to | 0xc2 << 24; }
    }
    to = square + 7;

    if (square != a7 && square_set[to] & mask) {
      c = board.square[to];
      list++->move = 070000 | square << 6 | to
        | (0x28 << 24) + capture_value[white_pawn][c];

      if (attack_knight[to] & board.piece[black_king]) {
        list++->move = 040000 | square << 6 | to
          | (0x1a << 24) + capture_value[white_pawn][c];
      }
    }
    to = square + 9;

    if (square != h7 && square_set[to] & mask) {
      c = board.square[to];
      list++->move = 070000 | square << 6 | to
        | (0x28 << 24) + capture_value[white_pawn][c];

      if (attack_knight[to] & board.piece[black_king]) {
        list++->move = 040000 | square << 6 | to
          | (0x1a << 24) + capture_value[white_pawn][c];
      }
    }
  }
  list->move = 0;
  return list;
}

type_move_list* white_ordinary(type_move_list* list) {
  const uint64_t empty_squares = ~board.occupied_total;
  uint64_t U, T, L = 0, next = 0;
  int to, square;
  const int king_square_rerun = board.black_king;

  if (position->castle & 0x1
    && ((board.occupied_total | position->black_attack) & 0x0000000000000060) == 0) {
    list++->move = 010000 | e1 << 6 | g1 | (square_set[g1] & 0 ? 0x8000 : 0)
      | history_table[white_king][g1] << 16;
  }

  if (position->castle & 0x2 && (board.occupied_total & 0x000000000000000e) == 0
    && (position->black_attack & 0x000000000000000c) == 0) {
    list++->move = 010000 | e1 << 6 | c1 | (square_set[c1] & 0 ? 0x8000 : 0)
      | history_table[white_king][c1] << 16;
  }
  const uint64_t K = attack_pawn_white[king_square_rerun];

  if (board.piece[white_queen] | board.piece[white_rook])
    L = bitboard_line_obscured[2][king_square_rerun][board.occupied_total
        >> line_turn[2][king_square_rerun] & 077]
      | bitboard_line_obscured[3][king_square_rerun][board.occupied_90_left
        >> line_turn[3][king_square_rerun] & 077];

  if (board.piece[white_queen]
    | (board.piece[white_king_bishop] | board.piece[white_queen_bishop]))
    next = bitboard_line_obscured[1][king_square_rerun][board.occupied_45_right
        >> line_turn[1][king_square_rerun] & 077]
      | bitboard_line_obscured[0][king_square_rerun][board.occupied_45_left
        >> line_turn[0][king_square_rerun] & 077];

  for (U = (board.piece[white_pawn] & 0x0000ffffffffff00) << 8 & empty_squares; U;
       U &= U - 1) {
    to = bsf(U);

    if ((to & 070) == 020 && board.square[(to + 8)] == 0) {
      (list++)->move = ((((to) - 8) << 6) | ((to) + 8)) | ((square_set[((to) + 8)] & (K)) ? 0x8000 : 0)
        | history_table[white_pawn][(to + 8)] << 16;
    }
    (list++)->move = ((((to) - 8) << 6) | to) | ((square_set[to] & (K)) ? 0x8000 : 0)
      | history_table[white_pawn][to] << 16;
  }

  for (U = board.piece[white_queen]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077]
      | (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
        | bitboard_line_obscured[0][square][board.occupied_45_left
          >> line_turn[0][square] & 077])) & empty_squares;

    while (T) {
      to = bsf(T);
      list++->move = square << 6 | to | (square_set[to] & (L | next) ? 0x8000 : 0)
        | history_table[white_queen][to] << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[white_rook]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left
        >> line_turn[3][square] & 077]) & empty_squares;

    while (T) {
      to = bsf(T);
      list++->move = square << 6 | to | (square_set[to] & L ? 0x8000 : 0)
        | history_table[white_rook][to] << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[white_king_bishop] | board.piece[white_queen_bishop]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left
        >> line_turn[0][square] & 077]) & empty_squares;

    while (T) {
      to = bsf(T);
      list++->move = square << 6 | to | (square_set[to] & next ? 0x8000 : 0)
        | history_table[(square_set[square] & 0xaa55aa55aa55aa55 ? white_queen_bishop : white_king_bishop)][
          to] << 16;
      T &= T - 1;
    }
  }
  square = bsf(board.piece[white_king]);
  T = attack_king[square] & empty_squares & ~position->black_attack;

  while (T) {
    to = bsf(T);
    list++->move = square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
      | history_table[white_king][to] << 16;
    T &= T - 1;
  }

  for (U = board.piece[white_knight]; U; U &= U - 1) {
    square = bsf(U);
    T = attack_knight[square] & empty_squares;

    while (T) {
      to = bsf(T);
      list++->move = square << 6 | to
        | (square_set[to] & attack_knight[king_square_rerun] ? 0x8000 : 0)
        | history_table[white_knight][to] << 16;
      T &= T - 1;
    }
  }

  for (U = board.piece[white_pawn] & 0x00ff000000000000; U; U &= U - 1) {
    square = bsf(U);
    to = square + 8;

    if (board.square[to] == 0) {
      if ((attack_knight[to] & board.piece[black_king]) == 0)
        list++->move = 040000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
          | history_table[white_pawn][to] << 16;
      list++->move = 060000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[white_pawn][to] << 16;
      list++->move = 050000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[white_pawn][to] << 16;
    }
    to = square + 7;

    if (square != a7 && square_set[to] & board.piece[occupied_black]) {
      if ((attack_knight[to] & board.piece[black_king]) == 0)
        list++->move = 040000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
          | history_table[white_pawn][to] << 16;
      list++->move = 060000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[white_pawn][to] << 16;
      list++->move = 050000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[white_pawn][to] << 16;
    }
    to = square + 9;

    if (square != h7 && square_set[to] & board.piece[occupied_black]) {
      if ((attack_knight[to] & board.piece[black_king]) == 0)
        list++->move = 040000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
          | history_table[white_pawn][to] << 16;
      list++->move = 060000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[white_pawn][to] << 16;
      list++->move = 050000 | square << 6 | to | (square_set[to] & 0 ? 0x8000 : 0)
        | history_table[white_pawn][to] << 16;
    }
  }
  list->move = 0;
  return list;
}

type_move_list* white_check(type_move_list* list, uint64_t mask) {
  int king_square_rerun, king_square, square, to, from, piece;
  uint64_t U, T, V;
  uint32_t move;
  uint64_t kr;
  kr = ~position->white_xray;
  mask = ~mask & ~board.piece[occupied_white];

  king_square = board.black_king;

  for (U = position->white_xray & board.piece[occupied_white]; U;
       U &= U - 1) {
    from = bsf(U);
    piece = board.square[from];

    if (piece == white_pawn) {
      if ((from & 7) != (king_square & 7) && from < a7 && board.square[(from + 8)] == 0) {
        list++->move = from << 6 | from + 8;

        if ((from & 070) == 010 && board.square[(from + 16)] == 0)
          list++->move = from << 6 | from + 16;
      }

      if ((square_set[from] & 0x7f7f7f7f7f7f7f7f)
        << 9 & board.piece[occupied_black] & mask)
        list++->move = from << 6 | from + 9;

      if ((square_set[from] & 0xfefefefefefefefe)
        << 7 & board.piece[occupied_black] & mask)
        list++->move = from << 6 | from + 7;
    }
    else if (piece == white_knight) {
      V = attack_knight[from] & mask;

      while (V) {
        to = bsf(V);
        list++->move = from << 6 | to;
        V &= V - 1;
      }
    }
    else if (piece == white_king_bishop || piece == white_queen_bishop) {
      V = (bitboard_line_obscured[1][from][board.occupied_45_right >> line_turn[1][from] & 077]
        | bitboard_line_obscured[0][from][board.occupied_45_left >> line_turn[0][from] & 077]) & mask;

      while (V) {
        to = bsf(V);
        list++->move = from << 6 | to;
        V &= V - 1;
      }
    }
    else if (piece == white_rook) {
      V = (bitboard_line_obscured[2][from][board.occupied_total >> line_turn[2][from] & 077]
        | bitboard_line_obscured[3][from][board.occupied_90_left >> line_turn[3][from] & 077]) & mask;

      while (V) {
        to = bsf(V);
        list++->move = from << 6 | to;
        V &= V - 1;
      }
    }
    else if (piece == white_king) {
      if ((from & 7) == (king_square & 7) || from >> 3 == king_square >> 3)
        V = attack_king[from] & non_orthogonal[king_square] & mask & ~position->black_attack;
      else
        V = attack_king[from] & non_diagonal[king_square] & mask & ~position->black_attack;

      while (V) {
        to = bsf(V);
        list++->move = from << 6 | to;
        V &= V - 1;
      }
    }
  }
  king_square_rerun = board.black_king;
  T = (board.piece[white_pawn] & ~0x0101010101010101)
    << 7 & ~0xff00000000000000 & mask & board.
    piece[occupied_black] & attack_pawn_white[king_square_rerun];

  while (T) {
    to = bsf(T);
    (list++)->move = (((to) - 7) << 6) | to;
    T &= T - 1;
  }
  T = (board.piece[white_pawn] & ~0x8080808080808080)
    << 9 & ~0xff00000000000000 & mask & board.
    piece[occupied_black] & attack_pawn_white[king_square_rerun];

  while (T) {
    to = bsf(T);
    (list++)->move = (((to) - 9) << 6) | to;
    T &= T - 1;
  }

  for (U = board.piece[white_queen]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left >> line_turn[3][square] & 077]
      | (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
        | bitboard_line_obscured[0][square][board.occupied_45_left
          >> line_turn[0][square] & 077])) & (bitboard_line_obscured[2][king_square][board.
        occupied_total >> line_turn[2][king_square] & 077]
      | bitboard_line_obscured[3][king_square][board.occupied_90_left
        >> line_turn[3][king_square] & 077]
      | (bitboard_line_obscured[1][king_square][board.occupied_45_right
          >> line_turn[1][king_square] & 077]
        | bitboard_line_obscured[0][king_square][board.occupied_45_left
          >> line_turn[0][king_square] & 077])) & mask;

    while (T) {
      to = bsf(T);
      T &= T - 1;

      if ((attack_pawn_black[to] & board.piece[black_pawn] & kr) == 0
        && (attack_knight[to] & board.piece[black_knight] & kr) == 0) {
        move = square << 6 | to;

        if (white_see(move))
          list++->move = square << 6 | to;
      }
    }
  }

  for (U = board.piece[white_rook]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[2][square][board.occupied_total >> line_turn[2][square] & 077]
      | bitboard_line_obscured[3][square][board.occupied_90_left
        >> line_turn[3][square] & 077]) & (bitboard_line_obscured[2][king_square][board.
        occupied_total >> line_turn[2][king_square] & 077]
      | bitboard_line_obscured[3][king_square][board.occupied_90_left
        >> line_turn[3][king_square] & 077]) & mask;

    while (T) {
      to = bsf(T);
      T &= T - 1;

      if ((attack_pawn_black[to] & board.piece[black_pawn] & kr) == 0
        && (attack_knight[to] & board.piece[black_knight] & kr) == 0) {
        move = square << 6 | to;

        if (white_see(move))
          list++->move = square << 6 | to;
      }
    }
  }

  for (U = board.piece[white_king_bishop] | board.piece[white_queen_bishop]; U; U &= U - 1) {
    square = bsf(U);
    T = (bitboard_line_obscured[1][square][board.occupied_45_right >> line_turn[1][square] & 077]
      | bitboard_line_obscured[0][square][board.occupied_45_left
        >> line_turn[0][square] & 077]) & (bitboard_line_obscured[1][king_square][board.
        occupied_45_right >> line_turn[1][king_square] & 077]
      | bitboard_line_obscured[0][king_square][board.occupied_45_left
        >> line_turn[0][king_square] & 077]) & mask;

    while (T) {
      to = bsf(T);
      T &= T - 1;

      if ((attack_pawn_black[to] & board.piece[black_pawn] & kr) == 0) {
        move = square << 6 | to;

        if (white_see(move))
          list++->move = square << 6 | to;
      }
    }
  }

  for (U = board.piece[white_knight]; U; U &= U - 1) {
    square = bsf(U);
    T = attack_knight[square] & attack_knight[king_square] & mask;

    while (T) {
      to = bsf(T);
      T &= T - 1;

      if ((attack_pawn_black[to] & board.piece[black_pawn] & kr) == 0) {
        move = square << 6 | to;

        if (white_see(move))
          list++->move = square << 6 | to;
      }
    }
  }

  if (board.piece[black_king] & 0x7f7f7f7f7f000000
    && board.square[(king_square_rerun - 7)] == 0) {
    if (board.square[(king_square_rerun - 15)] == white_pawn) {
      from = king_square_rerun - 15;
      to = king_square_rerun - 7;
      move = from << 6 | to;

      if ((position->white_attack & square_set[to]
        || bitboard_line_obscured[3][from][board.occupied_90_left
          >> line_turn[3][from] & 077] & (board.piece[white_rook]
          | board.piece[white_queen])) && white_see(move))
        list++->move = move;
    }

    if (king_square_rerun >> 3 == rank_5 && board.square[(king_square_rerun - 15)] == 0
      && board.square[(king_square_rerun - 23)] == white_pawn) {
      to = king_square_rerun - 7;
      from = king_square_rerun - 23;
      move = from << 6 | to;

      if ((position->white_attack & square_set[to]
        || bitboard_line_obscured[3][from][board.occupied_90_left
          >> line_turn[3][from] & 077] & (board.piece[white_rook]
          | board.piece[white_queen])) && white_see(move))
        list++->move = move;
    }
  }

  if (board.piece[black_king] & 0xfefefefefe000000
    && board.square[(king_square_rerun - 9)] == 0) {
    if (board.square[(king_square_rerun - 17)] == white_pawn) {
      from = king_square_rerun - 17;
      to = king_square_rerun - 9;
      move = from << 6 | to;

      if ((position->white_attack & square_set[to]
        || bitboard_line_obscured[3][from][board.occupied_90_left
          >> line_turn[3][from] & 077] & (board.piece[white_rook]
          | board.piece[white_queen])) && white_see(move))
        list++->move = move;
    }

    if (king_square_rerun >> 3 == rank_5 && board.square[(king_square_rerun - 17)] == 0
      && board.square[(king_square_rerun - 25)] == white_pawn) {
      to = king_square_rerun - 9;
      from = king_square_rerun - 25;
      move = from << 6 | to;

      if ((position->white_attack & square_set[to]
        || bitboard_line_obscured[3][from][board.occupied_90_left
          >> line_turn[3][from] & 077] & (board.piece[white_rook]
          | board.piece[white_queen])) && white_see(move))
        list++->move = move;
    }
  }
  list->move = 0;
  return list;
}

uint32_t black_next(type_next* next) {
  type_move_list *p, *q, *move_list;
  uint32_t move, temp;

  switch (next->phase) {
    case trans_value: next->phase = gen_captures;

      if (next->trans_move && black_ok(next->trans_move))
        return next->trans_move;
      [[fallthrough]];

    case gen_captures: next->phase = capture_moves;
      next->move = 0;
      black_capture(next->list, board.piece[occupied_white]);
      [[fallthrough]];

    case capture_moves: while (true) {
        p = next->list + next->move;
        move = p->move;

        if (!move)
          break;
        q = p + 1;
        next->move++;

        while (q->move) {
          if (move < q->move) {
            temp = q->move;
            q->move = move;
            move = temp;
          }
          q++;
        }

        if ((move & 0x7fff) == next->trans_move)
          continue;

        if (!(move & 0x300000) && !black_see(move))
          next->bad_captures[next->bad_capture++] = move;
        else
          break;
      }

      if (move)
        return move;
      next->phase = killer_1;
      move = position->killer_1;

      if (move && move != next->trans_move && board.square[(move & 077)] == 0
        && black_ok(move))
        return move;
      [[fallthrough]];

    case killer_1: next->phase = killer_2;
      move = position->killer_2;

      if (move && move != next->trans_move && board.square[(move & 077)] == 0
        && black_ok(move))
        return move;
      [[fallthrough]];

    case killer_2: next->phase = ordinary_moves;
      next->move = 0;
      move_list = black_ordinary(next->list);
      sort(next->list, move_list, next->trans_move, position->killer_1, position->killer_2);
      [[fallthrough]];

    case ordinary_moves: move = (next->list + next->move)->move;
      next->move++;

      if (move)
        return move;
      next->phase = bad_captures;
      next->bad_captures[next->bad_capture] = 0;
      next->move = 0;
      [[fallthrough]];

    case bad_captures: move = next->bad_captures[next->move++];
      return move;

    case trans_value_2: next->phase = gen_captures_2;

      if (next->trans_move && black_ok(next->trans_move))
        return next->trans_move;
      [[fallthrough]];

    case gen_captures_2: next->phase = capture_moves_2;
      next->move = 0;
      black_capture(next->list, next->target);
      [[fallthrough]];

    case capture_moves_2: while (true) {
        p = next->list + next->move;
        move = p->move;

        if (!move)
          break;
        q = p + 1;
        next->move++;

        while (q->move) {
          if (move < q->move) {
            temp = q->move;
            q->move = move;
            move = temp;
          }
          q++;
        }

        if ((move & 0x7fff) == next->trans_move)
          continue;
        break;
      }

      if (move)
        return move;
      next->move = 0;
      next->phase = quiet_checks;
      black_check(next->list, next->target);
      [[fallthrough]];

    case quiet_checks: move = (next->list + next->move)->move;
      next->move++;
      return move;

    case check_evasions: move = (next->list + next->move)->move;
      next->move++;
      return move;

    case trans_value_3: next->phase = gen_captures_3;

      if (next->trans_move && black_ok(next->trans_move))
        return next->trans_move;
      [[fallthrough]];

    case gen_captures_3: next->phase = capture_moves_3;
      next->move = 0;
      black_capture(next->list, board.piece[occupied_white]);
      [[fallthrough]];

    case capture_moves_3: while (true) {
        p = next->list + next->move;
        move = p->move;

        if (!move)
          break;
        q = p + 1;
        next->move++;

        while (q->move) {
          if (move < q->move) {
            temp = q->move;
            q->move = move;
            move = temp;
          }
          q++;
        }

        if ((move & 0x7fff) == next->trans_move)
          continue;
        break;
      }

      if (move)
        return move;
      next->move = 0;
      next->phase = quiet_checks_3;
      black_check(next->list, board.piece[occupied_white]);
      [[fallthrough]];

    case quiet_checks_3: move = (next->list + next->move)->move;
      next->move++;

      if (move)
        return move;
      next->move = 0;
      next->phase = phase;
      black_gain(next->list, next->mask);
      [[fallthrough]];

    case phase: move = (next->list + next->move)->move;
      next->move++;
      return move;
    default: ;
  }
  return 0;
}

uint32_t white_next(type_next* next) {
  type_move_list *p, *q, *move_list;
  uint32_t move, temp;

  switch (next->phase) {
    case trans_value: next->phase = gen_captures;

      if (next->trans_move && white_ok(next->trans_move))
        return next->trans_move;
      [[fallthrough]];

    case gen_captures: next->phase = capture_moves;
      next->move = 0;
      white_capture(next->list, board.piece[occupied_black]);
      [[fallthrough]];

    case capture_moves: while (true) {
        p = next->list + next->move;
        move = p->move;

        if (!move)
          break;
        q = p + 1;
        next->move++;

        while (q->move) {
          if (move < q->move) {
            temp = q->move;
            q->move = move;
            move = temp;
          }
          q++;
        }

        if ((move & 0x7fff) == next->trans_move)
          continue;

        if (!(move & 0x300000) && !white_see(move))
          next->bad_captures[next->bad_capture++] = move;
        else
          break;
      }

      if (move)
        return move;
      next->phase = killer_1;
      move = position->killer_1;

      if (move && move != next->trans_move && board.square[(move & 077)] == 0
        && white_ok(move))
        return move;
      [[fallthrough]];

    case killer_1: next->phase = killer_2;
      move = position->killer_2;

      if (move && move != next->trans_move && board.square[(move & 077)] == 0
        && white_ok(move))
        return move;
      [[fallthrough]];

    case killer_2: next->phase = ordinary_moves;
      next->move = 0;
      move_list = white_ordinary(next->list);
      sort(next->list, move_list, next->trans_move, position->killer_1,
           position->killer_2);
      [[fallthrough]];

    case ordinary_moves: move = (next->list + next->move)->move;
      next->move++;

      if (move)
        return move;
      next->phase = bad_captures;
      next->bad_captures[next->bad_capture] = 0;
      next->move = 0;
      [[fallthrough]];

    case bad_captures: move = next->bad_captures[next->move++];
      return move;

    case trans_value_2: next->phase = gen_captures_2;

      if (next->trans_move && white_ok(next->trans_move))
        return next->trans_move;
      [[fallthrough]];

    case gen_captures_2: next->phase = capture_moves_2;
      next->move = 0;
      white_capture(next->list, next->target);
      [[fallthrough]];

    case capture_moves_2: while (true) {
        p = next->list + next->move;
        move = p->move;

        if (!move)
          break;
        q = p + 1;
        next->move++;

        while (q->move) {
          if (move < q->move) {
            temp = q->move;
            q->move = move;
            move = temp;
          }
          q++;
        }

        if ((move & 0x7fff) == next->trans_move)
          continue;
        break;
      }

      if (move)
        return move;
      next->move = 0;
      next->phase = quiet_checks;
      white_check(next->list, next->target);
      [[fallthrough]];

    case quiet_checks: move = (next->list + next->move)->move;
      next->move++;
      return move;

    case check_evasions: move = (next->list + next->move)->move;
      next->move++;
      return move;

    case trans_value_3: next->phase = gen_captures_3;

      if (next->trans_move && white_ok(next->trans_move))
        return next->trans_move;
      [[fallthrough]];

    case gen_captures_3: next->phase = capture_moves_3;
      next->move = 0;
      white_capture(next->list, board.piece[occupied_black]);
      [[fallthrough]];

    case capture_moves_3: while (true) {
        p = next->list + next->move;
        move = p->move;

        if (!move)
          break;
        q = p + 1;
        next->move++;

        while (q->move) {
          if (move < q->move) {
            temp = q->move;
            q->move = move;
            move = temp;
          }
          q++;
        }

        if ((move & 0x7fff) == next->trans_move)
          continue;
        break;
      }

      if (move)
        return move;
      next->move = 0;
      next->phase = quiet_checks_3;
      white_check(next->list, board.piece[occupied_black]);
      [[fallthrough]];

    case quiet_checks_3: move = (next->list + next->move)->move;
      next->move++;

      if (move)
        return move;
      next->move = 0;
      next->phase = phase;
      white_gain(next->list, next->mask);
      [[fallthrough]];

    case phase: move = (next->list + next->move)->move;
      next->move++;
      return move;
    default: ;
  }
  return 0;
}

void black_top() {
  int value, hash_score = 0, move_depth = 0,
      exact_depth = 0;
  uint32_t move, hash_move = 0, exact_move = 0;
  type_move_list *p, *q, *move_list;
  const type_position* temp_position = position;
  const int capture_val[16] =
    {
      0, 1, 3, 0, 3, 3, 5, 9, 0, 1, 3, 0, 3, 3, 5, 9
    };

  eval(-0x7fff0000, 0x7fff0000, 0);

  if (board.white_to_move
        ? board.piece[white_king] & position->black_attack
        : board.piece[black_king] & position->white_attack)
    move_list = black_evasion(root_move_list, 0xffffffffffffffff);
  else {
    q = black_capture(root_move_list, board.piece[occupied_white]);
    move_list = black_ordinary(q);
    sort(move_list, q, 0, 0, 0);
  }
  const int k = position->hash_key & hash_mask;

  for (int i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (hash->flag & 16) {
        exact_depth = trans_depth;
        exact_move = move;
        hash_score = hash->score_high;
      }

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        hash_move = move;
      }
    }
  }

  if (exact_depth >= previous_depth - 6 && exact_move == hash_move && exact_move && ok_immediate
    && previous_depth >= 18 && black_ok(exact_move) && hash_score < 25000 && hash_score > -25000 && !analysis_mode) {
    best_score = hash_score;
    best_move = exact_move;
    best_depth = exact_depth;
    ok_immediate = 0;

    if (!(board.white_to_move
            ? board.piece[white_king] & position->black_attack
            : board.piece[black_king] & position->white_attack))
      value = black_exclude(hash_score - 50, previous_depth - 6, exact_move);
    else
      value = black_exclude_check(hash_score - 50, previous_depth - 6, exact_move);

    if (value < hash_score - 50)
      return;
  }
  ok_immediate = 1;
  q = root_move_list;

  for (p = root_move_list; p < move_list; p++) {
    move = p->move & 0x7fff;
    black_make(move);
    eval(-0x7fff0000, 0x7fff0000, 0);

    if (!(temp_position + 1)->black_king_check)
      q++->move = move & 0x7fff;
    black_undo(move);
  }
  q->move = 0;
  move_list = q;

  for (p = root_move_list; p < move_list; p++) {
    if (board.square[(p->move & 077)]) {
      const uint32_t to = board.square[(p->move & 077)];
      const uint32_t from = board.square[(p->move >> 6 & 077)];
      p->move |= 0xff000000 + ((16 * capture_val[to] - capture_val[from]) << 16);
    }
  }

  for (p = root_move_list; p < move_list; p++)
    if (p->move == hash_move)
      p->move |= 0xffff0000;

  for (p = move_list - 1; p >= root_move_list; p--) {
    move = p->move;

    for (q = p + 1; q < move_list; q++) {
      if (move < q->move)
        (q - 1)->move = q->move;
      else
        break;
    }
    q--;
    q->move = move;
  }
  int alpha = -30000;

  if (!root_move_list[0].move) {
    if (board.white_to_move
          ? board.piece[white_king] & position->black_attack
          : board.piece[black_king] & position->white_attack)
      best_score = alpha;
    else
      best_score = 0;
    best_move = 0;
    best_depth = 0;
    return;
  }

  for (int depth = 2; depth <= 250; depth += 2) {
    bad_move = 0;
    move_battle = 0;

    if (depth >= 14 && best_score <= 25000 && -25000 <= best_score) {
      int A = 8;
      alpha = best_score - A;
      int beta = best_score + A;

      if (alpha < -25000)
        alpha = -30000;

      if (beta > 25000)
        beta = 30000;
    redo:
      value = black_root(alpha, beta, depth);

      if (value > alpha && value < beta)
        goto end;

      if (value <= alpha) {
        alpha -= A;
        A += A / 2;
        best_score = alpha;
        goto redo;
      }
      beta += A;
      A += A / 2;
      best_score = beta;
      goto redo;
    }
    value = black_root(-30000, 30000, depth);
  end:
    if (depth == 2) {
      if (!root_move_list[1].move
        || root_move_list[0].move - root_move_list[1].move >= 200 << 16)
        move_easy = 1;
    }
    best_score_previous = best_score;
    check_if_done(depth / 2);
  }
}

int black_root(int alpha, int beta, const int depth) {
  int temp_value;
  int num_moves = 0, count = 0;
  int value, best_value = -32750;
  type_move_list *p, *q;
  const type_position* temp_position = position;
  uint32_t move;

  if (beta > 30000)
    beta = 30000;

  if (alpha < -30000)
    alpha = -30000;

  for (p = root_move_list; p->move; p++) {
    num_moves++;
    p->move &= 0x7fff;
  }
  const int previous_low = alpha;
  p = root_move_list;

  while ((move = p->move)) {
    black_make(move);
    eval(-0x7fff0000, 0x7fff0000, move);
    const int in_check = (temp_position + 1)->white_king_check != 0;
    const int new_depth = depth - (2 - in_check);

    if (best_value == -32750 || depth <= 2)
      value = -white_pv(-beta, -alpha, new_depth, in_check);
    else {
      if (in_check) {
        if (new_depth <= 7)
          value = -white_low_check(-alpha, new_depth);
        else
          value = -white_cut_check(-alpha, new_depth);
      }
      else {
        if (new_depth <= 7)
          value = -white_low(-alpha, new_depth);
        else
          value = -white_cut(-alpha, new_depth);
      }

      if (value > alpha) {
        move_battle = 1;
        move_easy = 0;
      }

      if (value > alpha)
        value = -white_pv(-alpha - 1, -alpha, new_depth, in_check);

      if (value > alpha)
        value = -white_pv(-beta, -alpha, new_depth, in_check);

      if (value <= alpha)
        value = alpha;
    }
    black_undo(move);

    if (value <= alpha)
      temp_value = previous_low;
    else
      temp_value = value;
    p->move |= (temp_value + 0x8000) << 16;

    if (value > best_value) {
      best_value = value;

      if (best_value == -32750 || value > alpha) {
        best_move = move;
        best_score = value;
        best_depth = depth;

        if (value > alpha && value < beta)
          information(get_time() - clock_start, value);

        if (value >= best_score_previous - 25)
          bad_move = 0;
        else {
          bad_move = 1;
          move_easy = 0;
        }
      }
    }

    if (value <= alpha) {
      if (count == 0) {
        bad_move = 1;
        move_easy = 0;
      }
    }
    else
      alpha = value;
    count++;

    if (value < beta) {
      p++;
      continue;
    }
    break;
  }

  for (p = root_move_list + (num_moves - 1); p >= root_move_list; p--) {
    move = p->move;

    for (q = p + 1; q < root_move_list + num_moves; q++) {
      if ((move & 0xffff0000) < (q->move & 0xffff0000))
        (q - 1)->move = q->move;
      else
        break;
    }
    q--;
    q->move = move;
  }
  best_depth = depth;
  return best_value;
}

void white_top() {
  int value, hash_score = 0, move_depth = 0,
      exact_depth = 0;
  uint32_t move, hash_move = 0, exact_move = 0;
  type_move_list *p, *q, *move_list;
  const type_position* temp_position = position;
  const int capture_val[16] =
    {
      0, 1, 3, 0, 3, 3, 5, 9, 0, 1, 3, 0, 3, 3, 5, 9
    };

  eval(-0x7fff0000, 0x7fff0000, 0);

  if (board.white_to_move
        ? board.piece[white_king] & position->black_attack
        : board.piece[black_king] & position->white_attack)
    move_list = white_evasion(root_move_list, 0xffffffffffffffff);
  else {
    q = white_capture(root_move_list, board.piece[occupied_black]);
    move_list = white_ordinary(q);
    sort(move_list, q, 0, 0, 0);
  }

  const int k = position->hash_key & hash_mask;

  for (int i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (hash->flag & 16) {
        exact_depth = trans_depth;
        exact_move = move;
        hash_score = hash->score_high;
      }

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        hash_move = move;
      }
    }
  }

  if (exact_depth >= previous_depth - 6 && exact_move == hash_move && exact_move && ok_immediate
    && previous_depth >= 18 && white_ok(exact_move) && hash_score < 25000 && hash_score > -25000 && !analysis_mode) {
    best_score = hash_score;
    best_move = exact_move;
    best_depth = exact_depth;
    ok_immediate = 0;

    if (!(board.white_to_move
            ? board.piece[white_king] & position->black_attack
            : board.piece[black_king] & position->white_attack))
      value = white_exclude(hash_score - 50, previous_depth - 6, exact_move);
    else
      value = white_exclude_check(hash_score - 50, previous_depth - 6, exact_move);

    if (value < hash_score - 50)
      return;
  }

  ok_immediate = 1;
  q = root_move_list;

  for (p = root_move_list; p < move_list; p++) {
    move = p->move & 0x7fff;
    white_make(move);
    eval(-0x7fff0000, 0x7fff0000, 0);

    if (!(temp_position + 1)->white_king_check)
      q++->move = move & 0x7fff;
    white_undo(move);
  }
  q->move = 0;
  move_list = q;

  for (p = root_move_list; p < move_list; p++) {
    if (board.square[(p->move & 077)]) {
      const uint32_t to = board.square[(p->move & 077)];
      const uint32_t from = board.square[(p->move >> 6 & 077)];
      p->move |= 0xff000000 + ((16 * capture_val[to] - capture_val[from]) << 16);
    }
  }

  for (p = root_move_list; p < move_list; p++)
    if (p->move == hash_move)
      p->move |= 0xffff0000;

  for (p = move_list - 1; p >= root_move_list; p--) {
    move = p->move;

    for (q = p + 1; q < move_list; q++) {
      if (move < q->move)
        (q - 1)->move = q->move;
      else
        break;
    }
    q--;
    q->move = move;
  }
  int alpha = -30000;

  if (!root_move_list[0].move) {
    if (board.white_to_move
          ? board.piece[white_king] & position->black_attack
          : board.piece[black_king] & position->white_attack)
      best_score = alpha;
    else
      best_score = 0;
    best_move = 0;
    best_depth = 0;
    return;
  }

  for (int depth = 2; depth <= 250; depth += 2) {
    bad_move = 0;
    move_battle = 0;

    if (depth >= 14 && best_score <= 25000 && -25000 <= best_score) {
      int A = 8;
      alpha = best_score - A;
      int beta = best_score + A;

      if (alpha < -25000)
        alpha = -30000;

      if (beta > 25000)
        beta = 30000;
    redo:
      value = white_root(alpha, beta, depth);

      if (value > alpha && value < beta)
        goto end;

      if (value <= alpha) {
        alpha -= A;
        A += A / 2;
        best_score = alpha;
        goto redo;
      }
      beta += A;
      A += A / 2;
      best_score = beta;
      goto redo;
    }
  end:
    if (depth == 2) {
      if (!root_move_list[1].move
        || root_move_list[0].move - root_move_list[1].move >= 200 << 16)
        move_easy = 1;
    }
    best_score_previous = best_score;
    check_if_done(depth / 2);
  }
}

int white_root(int alpha, int beta, const int depth) {
  int temp_value;
  int num_moves = 0, count = 0;
  int value, best_value = -32750;
  type_move_list *p, *q;
  const type_position* temp_position = position;
  uint32_t move;

  if (beta > 30000)
    beta = 30000;

  if (alpha < -30000)
    alpha = -30000;

  for (p = root_move_list; p->move; p++) {
    num_moves++;
    p->move &= 0x7fff;
  }
  const int previous_low = alpha;
  p = root_move_list;

  while ((move = p->move)) {
    white_make(move);
    eval(-0x7fff0000, 0x7fff0000, move);
    const int in_check = (temp_position + 1)->black_king_check != 0;
    const int new_depth = depth - (2 - in_check);

    if (best_value == -32750 || depth <= 2)
      value = -black_pv(-beta, -alpha, new_depth, in_check);
    else {
      if (in_check) {
        if (new_depth <= 7)
          value = -black_low_check(-alpha, new_depth);
        else
          value = -black_cut_check(-alpha, new_depth);
      }
      else {
        if (new_depth <= 7)
          value = -black_low(-alpha, new_depth);
        else
          value = -black_cut(-alpha, new_depth);
      }

      if (value > alpha) {
        move_battle = 1;
        move_easy = 0;
      }

      if (value > alpha)
        value = -black_pv(-alpha - 1, -alpha, new_depth, in_check);

      if (value > alpha)
        value = -black_pv(-beta, -alpha, new_depth, in_check);

      if (value <= alpha)
        value = alpha;
    }
    white_undo(move);

    if (value <= alpha)
      temp_value = previous_low;
    else
      temp_value = value;
    p->move |= (temp_value + 0x8000) << 16;

    if (value > best_value) {
      best_value = value;

      if (best_value == -32750 || value > alpha) {
        best_move = move;
        best_score = value;
        best_depth = depth;

        if (value > alpha && value < beta)
          information(get_time() - clock_start, value);

        if (value >= best_score_previous - 25)
          bad_move = 0;
        else {
          bad_move = 1;
          move_easy = 0;
        }
      }
    }

    if (value <= alpha) {
      if (count == 0) {
        bad_move = 1;
        move_easy = 0;
      }
    }
    else
      alpha = value;
    count++;

    if (value < beta) {
      p++;
      continue;
    }
    break;
  }

  for (p = root_move_list + (num_moves - 1); p >= root_move_list; p--) {
    move = p->move;

    for (q = p + 1; q < root_move_list + num_moves; q++) {
      if ((move & 0xffff0000) < (q->move & 0xffff0000))
        (q - 1)->move = q->move;
      else
        break;
    }
    q--;
    q->move = move;
  }
  best_depth = depth;
  return best_value;
}

int black_pv(int alpha, int beta, int depth, int check_node) {
  type_next next[1];
  type_hash* hash;
  int good_move, value, hash_score, k, i, trans_depth, move, move_depth = 0,
      trans_move = 0, hash_depth, singular = 0;
  int extend, best_value, new_depth, in_check, to;
  type_position* temp_position = position;

  if (beta < -30000)
    return -30000;

  if (alpha > 30000)
    return 30000;

  if (depth <= 1) {
    if (check_node)
      return black_qsearch_pv_check(alpha, beta, 1);
    return black_qsearch_pv(alpha, beta, 1);
  }

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  next->trans_move = 0;
  hash_depth = 0;
  next->move = 0;
  next->bad_capture = 0;
  k = position->hash_key & hash_mask;
  (temp_position + 1)->move = 0;

  for (i = 0; i < 4; i++) {
    hash = hash_table + (k + i);

    if ((hash->hash_key ^ position->hash_key >> 32) == 0) {
      trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low > hash->depth_high) {
        trans_depth = hash->depth_low;
        hash_score = hash->score_low;
      }
      else {
        trans_depth = hash->depth_high;
        hash_score = hash->score_high;
      }

      if (trans_depth > hash_depth)
        hash_depth = trans_depth;

      if (hash->flag & 16 && trans_depth >= depth) {
        hash->age = age;

        if (!analysis_mode)
          return hash_score;
      }
    }
  }

  if (!trans_move && depth >= 6) {
    value = alpha;

    if (depth >= 10) {
      value = black_pv(alpha - depth, beta + depth, depth - 8, check_node);

      if (value > alpha - depth)
        trans_move = (temp_position + 1)->move;
    }

    if (value > alpha - depth)
      value = black_pv(alpha - depth, beta + depth, depth - 4, check_node);

    if (value > alpha - depth)
      trans_move = (temp_position + 1)->move;
  }
  else if (depth >= 10 && depth > hash_depth + 8) {
    value = black_pv(alpha - depth, beta + depth, depth - 8, check_node);

    if (value > alpha - depth)
      trans_move = (temp_position + 1)->move;

    if (value > alpha - depth) {
      value = black_pv(alpha - depth, beta + depth, depth - 4, check_node);

      if (value > alpha - depth)
        trans_move = (temp_position + 1)->move;
    }
  }
  next->trans_move = trans_move;
  next->phase = trans_value;
  extend = 0;
  next->target = board.piece[occupied_white];

  if (check_node) {
    type_move_list* q;
    type_move_list* p;
    type_move_list* move_list;
    move_list = black_evasion(next->list, 0xffffffffffffffff);
    next->phase = check_evasions;

    for (p = move_list - 1; p >= next->list; p--) {
      if ((p->move & 0x7fff) == trans_move)
        p->move |= 0xffff0000;
      else if (p->move <= 0x80 << 24) {
        if ((p->move & 0x7fff) == temp_position->killer_1)
          p->move |= 0x7fff8000;

        else if ((p->move & 0x7fff) == temp_position->killer_2)
          p->move |= 0x7fff0000;

        else
          p->move |= (p->move & 0x7fff)
            | history_table[board.square[(p->move >> 6 & 077)]][(p->move & 077)] << 15;
      }
      move = p->move;

      for (q = p + 1; q < move_list; q++) {
        if (move < q->move)
          (q - 1)->move = q->move;
        else
          break;
      }
      q--;
      q->move = move;
    }

    if (move_list - next->list <= 1)
      singular = 2;

    if (move_list - next->list == 2)
      singular = 1;

    if (move_list - next->list > 2)
      singular = 0;
  }

  if (depth >= 16 && next->trans_move && singular < 2 && black_ok(next->trans_move)) {
    move = next->trans_move;
    to = move & 077;
    black_make(move);
    eval(-0x7fff0000, 0x7fff0000, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      goto zab;
    }
    hash_score = -white_pv(-beta, -alpha, depth - 10, (temp_position + 1)->white_king_check != 0);
    black_undo(move);

    if (check_node)
      value = black_exclude_check(hash_score - depth / 2,
                                  depth - (12 <= depth / 2 ? 12 : depth / 2), move & 0x7fff);
    else
      value = black_exclude(hash_score - depth / 2, depth - (12 <= depth / 2 ? 12 : depth / 2),
                            move & 0x7fff);

    if (value < hash_score - depth / 2) {
      singular = 1;

      if (check_node)
        value = black_exclude_check(hash_score - depth,
                                    depth - (12 <= depth / 2 ? 12 : depth / 2), move & 0x7fff);
      else
        value = black_exclude(hash_score - depth, depth - (12 <= depth / 2 ? 12 : depth / 2),
                              move & 0x7fff);

      if (value < hash_score - depth)
        singular = 2;
    }
  }
zab:
  best_value = -32750;
  next->move = 0;
  next->bad_capture = 0;
  good_move = 0;

  while ((move = black_next(next))) {
    to = move & 077;

    if (alpha > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      best_value = 0 >= best_value ? 0 : best_value;
      continue;
    }
    move &= 0x7fff;
    black_make(move);
    eval(-0x7fff0000, 0x7fff0000, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }
    in_check = (temp_position + 1)->white_king_check != 0;
    extend = 0;

    if (extend < 2) {
      if (board.square[to] == black_pawn && to <= h3
        && (board.piece[white_pawn] & passed_pawn_black[to]) == 0)
        extend = 2;
    }

    if (extend < 2) {
      if ((temp_position + 1)->capture != 0
        || in_check
        || check_node && (position->material & 0xff) >= 18
        || (board.square[to] == black_pawn && to <= h5
          && (board.piece[white_pawn] & passed_pawn_black[to]) == 0))
        extend = 1;
    }

    if (next->trans_move != move)
      singular = 0;
    new_depth = depth - 2 + (extend >= singular ? extend : singular);

    if (next->trans_move != move && new_depth > 1) {
      if (new_depth <= 7) {
        if (in_check)
          value = -white_low_check(-alpha, new_depth);
        else
          value = -white_low(-alpha, new_depth);
      }
      else {
        if (in_check)
          value = -white_cut_check(-alpha, new_depth);
        else
          value = -white_cut(-alpha, new_depth);
      }

      if (value > alpha)
        value = -white_pv(-beta, -alpha, new_depth, in_check);
    }
    else
      value = -white_pv(-beta, -alpha, new_depth, in_check);
    black_undo(move);

    if (value <= alpha && board.square[(move & 077)] == 0 && (move & 060000) == 0) {
      int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];

      if (temp_position->score > alpha - 50)
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist - (ist * depth >> 8);
    }

    if (value <= best_value)
      continue;
    best_value = value;

    if (value <= alpha)
      continue;
    alpha = value;
    good_move = move;
    hash_low(position->hash_key, move, depth, value);

    if (value >= beta) {
      if (board.square[(move & 077)] == 0 && (move & 060000) == 0) {
        int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist + ((0xff00 - ist) * depth >> 8);

        if (move != position->killer_1) {
          position->killer_2 = position->killer_1;
          position->killer_1 = move;
        }
      }
      return value;
    }
  }
  move = good_move;
  (temp_position + 1)->move = good_move & 0x7fff;

  if (best_value == -32750) {
    if (!check_node)
      return 0;
    return temp_position - (root_position + 1) - 30000;
  }

  if (move) {
    if (board.square[(move & 077)] == 0 && (move & 060000) == 0) {
      int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
      history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
        ist + ((0xff00 - ist) * depth >> 8);

      if (move != position->killer_1) {
        position->killer_2 = position->killer_1;
        position->killer_1 = move;
      }
    }
    hash_exact(move, depth, best_value, 16);
    return best_value;
  }
  hash_high(position->hash_key, depth, best_value);
  return best_value;
}

int white_pv(int alpha, int beta, int depth, int check_node) {
  type_next next[1];
  type_hash* hash;
  int good_move, value, hash_score, k, i, trans_depth, move, move_depth = 0,
      trans_move = 0, hash_depth, singular = 0;
  int extend, best_value, new_depth, in_check, to;
  type_position* temp_position = position;

  if (beta < -30000)
    return -30000;

  if (alpha > 30000)
    return 30000;

  if (depth <= 1) {
    if (check_node)
      return white_qsearch_pv_check(alpha, beta, 1);
    return white_qsearch_pv(alpha, beta, 1);
  }

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  next->trans_move = 0;
  hash_depth = 0;
  next->move = 0;
  next->bad_capture = 0;
  k = static_cast<int>(position->hash_key & hash_mask);
  (temp_position + 1)->move = 0;

  for (i = 0; i < 4; i++) {
    hash = hash_table + (k + i);

    if ((hash->hash_key ^ position->hash_key >> 32) == 0) {
      trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low > hash->depth_high) {
        trans_depth = hash->depth_low;
        hash_score = hash->score_low;
      }
      else {
        trans_depth = hash->depth_high;
        hash_score = hash->score_high;
      }

      if (trans_depth > hash_depth)
        hash_depth = trans_depth;

      if (hash->flag & 16 && trans_depth >= depth) {
        hash->age = age;

        if (!analysis_mode)
          return hash_score;
      }
    }
  }

  if (!trans_move && depth >= 6) {
    value = alpha;

    if (depth >= 10) {
      value = white_pv(alpha - depth, beta + depth, depth - 8, check_node);

      if (value > alpha - depth)
        trans_move = (temp_position + 1)->move;
    }

    if (value > alpha - depth)
      value = white_pv(alpha - depth, beta + depth, depth - 4, check_node);

    if (value > alpha - depth)
      trans_move = (temp_position + 1)->move;
  }
  else if (depth >= 10 && depth > hash_depth + 8) {
    value = white_pv(alpha - depth, beta + depth, depth - 8, check_node);

    if (value > alpha - depth)
      trans_move = (temp_position + 1)->move;

    if (value > alpha - depth) {
      value = white_pv(alpha - depth, beta + depth, depth - 4, check_node);

      if (value > alpha - depth)
        trans_move = (temp_position + 1)->move;
    }
  }
  next->trans_move = trans_move;
  next->phase = trans_value;
  extend = 0;
  next->target = board.piece[occupied_black];

  if (check_node) {
    type_move_list* q;
    type_move_list* p;
    type_move_list* move_list;
    move_list = white_evasion(next->list, 0xffffffffffffffff);
    next->phase = check_evasions;

    for (p = move_list - 1; p >= next->list; p--) {
      if ((p->move & 0x7fff) == trans_move)
        p->move |= 0xffff0000;
      else if (p->move <= 0x80 << 24) {
        if ((p->move & 0x7fff) == temp_position->killer_1)
          p->move |= 0x7fff8000;

        else if ((p->move & 0x7fff) == temp_position->killer_2)
          p->move |= 0x7fff0000;

        else
          p->move |= (p->move & 0x7fff)
            | history_table[board.square[(p->move >> 6 & 077)]][(p->move & 077)] << 15;
      }
      move = p->move;

      for (q = p + 1; q < move_list; q++) {
        if (move < q->move)
          (q - 1)->move = q->move;
        else
          break;
      }
      q--;
      q->move = move;
    }

    if (move_list - next->list <= 1)
      singular = 2;

    if (move_list - next->list == 2)
      singular = 1;

    if (move_list - next->list > 2)
      singular = 0;
  }

  if (depth >= 16 && next->trans_move && singular < 2 && white_ok(next->trans_move)) {
    move = next->trans_move;
    to = move & 077;
    white_make(move);
    eval(-0x7fff0000, 0x7fff0000, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      goto zab;
    }
    hash_score = -black_pv(-beta, -alpha, depth - 10, (temp_position + 1)->black_king_check != 0);
    white_undo(move);

    if (check_node)
      value = white_exclude_check(hash_score - depth / 2,
                                  depth - (12 <= depth / 2 ? 12 : depth / 2), move & 0x7fff);
    else
      value = white_exclude(hash_score - depth / 2, depth - (12 <= depth / 2 ? 12 : depth / 2),
                            move & 0x7fff);

    if (value < hash_score - depth / 2) {
      singular = 1;

      if (check_node)
        value = white_exclude_check(hash_score - depth,
                                    depth - (12 <= depth / 2 ? 12 : depth / 2), move & 0x7fff);
      else
        value = white_exclude(hash_score - depth, depth - (12 <= depth / 2 ? 12 : depth / 2),
                              move & 0x7fff);

      if (value < hash_score - depth)
        singular = 2;
    }
  }
zab:
  best_value = -32750;
  next->move = 0;
  next->bad_capture = 0;
  good_move = 0;

  while ((move = white_next(next))) {
    to = move & 077;

    if (alpha > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      best_value = 0 >= best_value ? 0 : best_value;
      continue;
    }
    move &= 0x7fff;
    white_make(move);
    eval(-0x7fff0000, 0x7fff0000, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }
    in_check = (temp_position + 1)->black_king_check != 0;
    extend = 0;

    if (extend < 2) {
      if (board.square[to] == white_pawn && to >= a6
        && (board.piece[black_pawn] & passed_pawn_white[to]) == 0)
        extend = 2;
    }

    if (extend < 2) {
      if ((temp_position + 1)->capture != 0
        || in_check
        || check_node && (position->material & 0xff) >= 18
        || (board.square[to] == white_pawn && to >= a4
          && (board.piece[black_pawn] & passed_pawn_white[to]) == 0))
        extend = 1;
    }

    if (next->trans_move != move)
      singular = 0;
    new_depth = depth - 2 + (extend >= singular ? extend : singular);

    if (next->trans_move != move && new_depth > 1) {
      if (new_depth <= 7) {
        if (in_check)
          value = -black_low_check(-alpha, new_depth);
        else
          value = -black_low(-alpha, new_depth);
      }
      else {
        if (in_check)
          value = -black_cut_check(-alpha, new_depth);
        else
          value = -black_cut(-alpha, new_depth);
      }

      if (value > alpha)
        value = -black_pv(-beta, -alpha, new_depth, in_check);
    }
    else
      value = -black_pv(-beta, -alpha, new_depth, in_check);
    white_undo(move);

    if (value <= alpha && board.square[(move & 077)] == 0 && (move & 060000) == 0) {
      int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];

      if (temp_position->score > alpha - 50)
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist - (ist * depth >> 8);
    }

    if (value <= best_value)
      continue;
    best_value = value;

    if (value <= alpha)
      continue;
    alpha = value;
    good_move = move;
    hash_low(position->hash_key, move, depth, value);

    if (value >= beta) {
      if (board.square[(move & 077)] == 0 && (move & 060000) == 0) {
        int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist + ((0xff00 - ist) * depth >> 8);

        if (move != position->killer_1) {
          position->killer_2 = position->killer_1;
          position->killer_1 = move;
        }
      }
      return value;
    }
  }
  move = good_move;
  (temp_position + 1)->move = good_move & 0x7fff;

  if (best_value == -32750) {
    if (!check_node)
      return 0;
    return temp_position - (root_position + 1) - 30000;
  }

  if (move) {
    if (board.square[(move & 077)] == 0 && (move & 060000) == 0) {
      int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
      history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
        ist + ((0xff00 - ist) * depth >> 8);

      if (move != position->killer_1) {
        position->killer_2 = position->killer_1;
        position->killer_1 = move;
      }
    }
    hash_exact(move, depth, best_value, 16);
    return best_value;
  }
  hash_high(position->hash_key, depth, best_value);
  return best_value;
}

int black_cut(const int score, const int depth) {
  int move, i;
  int move_depth = 0, trans_move = 0, hash_score;
  int value, new_depth, in_check;
  type_next next[1];
  type_position* temp_position = position;
  const uint64_t hash_key = position->hash_key;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          if ((hash->flag & 8) != 8)
            if (position->flag & 1 || move) {
              hash->age = age;
              return hash_score;
            }
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          hash->age = age;
          return hash_score;
        }
      }
    }
  }
  next->trans_move = trans_move;

  if (temp_position->score >= score && position->flag & 1) {
    do_null();
    new_depth = depth - 8;
    new_depth -= static_cast<uint32_t>((temp_position->score - score <= 96 ? temp_position->score - score : 96)) /
      32;

    if (new_depth <= 1)
      value = -white_qsearch(1 - score, 0);

    else if (new_depth <= 7)
      value = -white_low(1 - score, new_depth);

    else
      value = -white_all(1 - score, new_depth);
    undo_null();

    if (value >= score) {
      if (trans_move == 0)
        hash_low(position->hash_key, 0, depth, value);
      return value;
    }
  }

  if (trans_move == 0 && depth >= 6) {
    if (depth < 12)
      value = black_low(score, depth - 4);
    else
      value = black_cut(score, depth - 4);

    if (value >= score)
      trans_move = (temp_position + 1)->move;
  }
  int singular = 0;

  if (depth >= 16 && trans_move && black_ok(trans_move)) {
    value = black_exclude(score - depth, depth - (12 <= depth / 2 ? 12 : depth / 2),
                          trans_move & 0x7fff);

    if (value < score - depth) {
      singular++;
      const int height = temp_position - (root_position + 1);

      if (height * 4 <= depth)
        singular++;
      value = black_exclude(score - 2 * depth, depth - (12 <= depth / 2 ? 12 : depth / 2),
                            trans_move & 0x7fff);

      if (value < score - 2 * depth) {
        singular++;

        if (height * 8 <= depth)
          singular++;
      }
    }
  }

  int count = 0;
  next->trans_move = trans_move;
  next->phase = trans_value;
  next->target = board.piece[occupied_white];

  if (depth < 20 && score - temp_position->score >= 48 * (depth - 5)) {
    next->phase = trans_value_2;
    count = 1;

    if (score - temp_position->score >= 48 * (depth - 2))
      next->target ^= board.piece[white_pawn];
  }
  next->move = 0;
  next->bad_capture = 0;

  while ((move = black_next(next))) {
    const int to = move & 077;
    const int from = move >> 6 & 077;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      count++;
      continue;
    }

    if (count > 5 && next->phase == ordinary_moves && (move & 0xe000) == 0
      && square_set[from] & ~position->black_xray && depth < 20) {
      if ((1 << (depth - 6))
        + static_cast<int>(max_increase[board.square[(move >> 6 & 077)]][move & 07777])
        + temp_position->score < score + 35 + 2 * count) {
        count++;
        continue;
      }
    }
    move &= 0x7fff;
    black_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check)
      in_check = 1;
    else
      in_check = 0;

    if (move != next->trans_move)
      singular = 0;
    int extend = 0;

    if (move == next->trans_move) {
      if (board.square[to] == black_pawn && to <= h5
        && (board.piece[white_pawn] & passed_pawn_black[to]) == 0)
        extend = 1;
    }
    else {
      if (board.square[to] == black_pawn && to <= h3
        && (board.piece[white_pawn] & passed_pawn_black[to]) == 0)
        extend = 1;
    }

    if (next->trans_move == move
      && ((temp_position + 1 - 1)->move & 077) == ((temp_position + 1)->move & 077)
      && (temp_position + 1 - 1)->capture != 0)
      extend++;
    extend = extend >= singular ? extend : singular;

    if (in_check) {
      new_depth = depth - 2 + (1 >= extend ? 1 : extend);
      value = -white_all_check(1 - score, new_depth);
    }
    else {
      if (count > 2 && depth < 20 && (temp_position + 1)->capture == 0
        && (2 << (depth - 6)) - (temp_position + 1)->score < score + count - 15) {
        black_undo(move);
        count++;
        continue;
      }

      if (next->phase == ordinary_moves && !extend) {
        new_depth = depth - 2 + extend - (4 + bsr(4 + count));

        if (new_depth <= 1)
          value = -white_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -white_low(1 - score, new_depth);

        else
          value = -white_all(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }
      new_depth = depth - 2 + extend;

      if (new_depth <= 7)
        value = -white_low(1 - score, new_depth);
      else
        value = -white_all(1 - score, new_depth);
    }
  exit_loop:
    black_undo(move);
    count++;

    if (value >= score) {
      if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
        const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist + ((0xff00 - ist) * depth >> 8);

        if (move != position->killer_1) {
          position->killer_2 = position->killer_1;
          position->killer_1 = move;
        }
      }
      hash_low(position->hash_key, move, depth, value);
      return value;
    }

    if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
      const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];

      if (temp_position->score > score - 50)
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist - (ist * depth >> 8);
    }
  }

  if (!count && next->phase <= trans_value_2)
    return 0;
  value = score - 1;
  hash_high_cut(depth, value);
  return value;
}

int black_cut_check(const int score, const int depth) {
  int move, reduction, extend;
  int move_depth = 0, trans_move = 0, hash_score, new_depth, value, i;
  type_move_list list[256], *q;
  const uint64_t hash_key = position->hash_key;
  type_position* temp_position = position;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          if ((hash->flag & 8) != 8) {
            hash->age = age;
            return hash_score;
          }
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          hash->age = age;
          return hash_score;
        }
      }
    }
  }

  if (trans_move && !black_ok(trans_move))
    trans_move = 0;
  int best_value = temp_position - (root_position + 1) - 30000;
  int singular = 0;

  if (depth >= 16 && trans_move) {
    value = black_exclude_check(score - depth, depth - (12 <= depth / 2 ? 12 : depth / 2),
                                trans_move & 0x7fff);

    if (value < score - depth) {
      singular++;
      const int height = temp_position - (root_position + 1);

      if (height * 4 <= depth)
        singular++;
      value = black_exclude_check(score - 2 * depth,
                                  depth - (12 <= depth / 2 ? 12 : depth / 2), trans_move & 0x7fff);

      if (value < score - 2 * depth) {
        singular++;

        if (height * 8 <= depth)
          singular++;
      }
    }
  }
  type_move_list* p = list;
  list[0].move = trans_move;
  int count = 0;
  uint8_t gen = 0;
  list[1].move = 0;

  while (p->move || !gen) {
    if (!p->move) {
      type_move_list* move_list = black_evasion(list + 1, 0xffffffffffffffff);
      gen = 1;

      for (p = move_list - 1; p >= list + 1; p--) {
        if ((p->move & 0x7fff) == trans_move)
          p->move = 0;
        else if (p->move <= 0x80 << 24) {
          if ((p->move & 0x7fff) == temp_position->killer_1)
            p->move |= 0x7fff8000;

          else if ((p->move & 0x7fff) == temp_position->killer_2)
            p->move |= 0x7fff0000;

          else
            p->move |= (p->move & 0x7fff)
              | history_table[board.square[(p->move >> 6 & 077)]][(p->move & 077)]
              << 15;
        }
        move = p->move;

        for (q = p + 1; q < move_list; q++) {
          if (move < q->move)
            (q - 1)->move = q->move;
          else
            break;
        }
        q--;
        q->move = move;
      }
      p = list + 1;
      continue;
    }
    move = p->move & 0x7fff;
    p++;

    if (move != trans_move)
      singular = 0;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      count++;
      best_value = 0 >= best_value ? 0 : best_value;
      continue;
    }
    black_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check) {
      new_depth = depth - 2;

      if (singular)
        new_depth += singular;
      else
        new_depth++;

      if (new_depth <= 7)
        value = -white_low_check(1 - score, new_depth);
      else
        value = -white_all_check(1 - score, new_depth);
    }
    else {
      if (count >= 1) {
        if (depth > 8)
          reduction = bsr(depth - 7);
        else
          reduction = 0;
        reduction += 1 + (count <= 2 ? count : 2);

        if ((position->material & 0xff) >= 18)
          extend = 1;
        else
          extend = 0;
        new_depth = depth + extend - reduction - 2;

        if (new_depth <= 1)
          value = -white_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -white_low(1 - score, new_depth);

        else
          value = -white_all(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }

      if (!singular && (position->material & 0xff) >= 18)
        extend = 1;
      else
        extend = 0;
      new_depth = depth - 2 + extend + singular;

      if (new_depth <= 7)
        value = -white_low(1 - score, new_depth);
      else
        value = -white_all(1 - score, new_depth);
    }
  exit_loop:
    black_undo(move);

    if (value > best_value)
      best_value = value;

    if (value < score) {
      count++;
      continue;
    }
    hash_low(position->hash_key, move, 1 >= depth ? 1 : depth, value);
    return value;
  }
  hash_high_cut(1 >= depth ? 1 : depth, best_value);
  return best_value;
}

int white_cut(const int score, const int depth) {
  int move, i;
  int move_depth = 0, trans_move = 0, hash_score;
  int value, new_depth, in_check;
  type_next next[1];
  type_position* temp_position = position;
  const uint64_t hash_key = position->hash_key;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          if ((hash->flag & 8) != 8)
            if (position->flag & 2 || move) {
              hash->age = age;
              return hash_score;
            }
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          hash->age = age;
          return hash_score;
        }
      }
    }
  }
  next->trans_move = trans_move;

  if (temp_position->score >= score && position->flag & 2) {
    do_null();
    new_depth = depth - 8;
    new_depth -=
      static_cast<uint32_t>((temp_position->score - score <= 96 ? temp_position->score - score : 96))
      / 32;

    if (new_depth <= 1)
      value = -black_qsearch(1 - score, 0);

    else if (new_depth <= 7)
      value = -black_low(1 - score, new_depth);

    else
      value = -black_all(1 - score, new_depth);
    undo_null();

    if (value >= score) {
      if (trans_move == 0)
        hash_low(position->hash_key, 0, depth, value);
      return value;
    }
  }

  if (trans_move == 0 && depth >= 6) {
    if (depth < 12)
      value = white_low(score, depth - 4);
    else
      value = white_cut(score, depth - 4);

    if (value >= score)
      trans_move = (temp_position + 1)->move;
  }
  int singular = 0;

  if (depth >= 16 && trans_move && white_ok(trans_move)) {
    value = white_exclude(score - depth, depth - (12 <= depth / 2 ? 12 : depth / 2),
                          trans_move & 0x7fff);

    if (value < score - depth) {
      singular++;
      const int height = temp_position - (root_position + 1);

      if (height * 4 <= depth)
        singular++;
      value = white_exclude(score - 2 * depth, depth - (12 <= depth / 2 ? 12 : depth / 2),
                            trans_move & 0x7fff);

      if (value < score - 2 * depth) {
        singular++;

        if (height * 8 <= depth)
          singular++;
      }
    }
  }
  int count = 0;
  next->trans_move = trans_move;
  next->phase = trans_value;
  next->target = board.piece[occupied_black];

  if (depth < 20 && score - temp_position->score >= 48 * (depth - 5)) {
    next->phase = trans_value_2;
    count = 1;

    if (score - temp_position->score >= 48 * (depth - 2))
      next->target ^= board.piece[black_pawn];
  }
  next->move = 0;
  next->bad_capture = 0;

  while ((move = white_next(next))) {
    const int to = move & 077;
    const int from = move >> 6 & 077;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      count++;
      continue;
    }

    if (count > 5 && next->phase == ordinary_moves && (move & 0xe000) == 0
      && square_set[from] & ~position->white_xray && depth < 20) {
      if ((1 << (depth - 6))
        + static_cast<int>(max_increase[board.square[(move >> 6 & 077)]][move & 07777])
        + temp_position->score < score + 35 + 2 * count) {
        count++;
        continue;
      }
    }
    move &= 0x7fff;
    white_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check)
      in_check = 1;
    else
      in_check = 0;

    if (move != next->trans_move)
      singular = 0;
    int extend = 0;

    if (move == next->trans_move) {
      if (board.square[to] == white_pawn && to >= a4
        && (board.piece[black_pawn] & passed_pawn_white[to]) == 0)
        extend = 1;
    }
    else {
      if (board.square[to] == white_pawn && to >= a6
        && (board.piece[black_pawn] & passed_pawn_white[to]) == 0)
        extend = 1;
    }

    if (next->trans_move == move
      && ((temp_position + 1 - 1)->move & 077) == ((temp_position + 1)->move & 077)
      && (temp_position + 1 - 1)->capture != 0)
      extend++;
    extend = extend >= singular ? extend : singular;

    if (in_check) {
      new_depth = depth - 2 + (1 >= extend ? 1 : extend);
      value = -black_all_check(1 - score, new_depth);
    }
    else {
      if (count > 2 && depth < 20 && (temp_position + 1)->capture == 0
        && (2 << (depth - 6)) - (temp_position + 1)->score < score + count - 15) {
        white_undo(move);
        count++;
        continue;
      }

      if (next->phase == ordinary_moves && !extend) {
        new_depth = depth - 2 + extend - (4 + bsr(4 + count));

        if (new_depth <= 1)
          value = -black_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -black_low(1 - score, new_depth);

        else
          value = -black_all(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }
      new_depth = depth - 2 + extend;

      if (new_depth <= 7)
        value = -black_low(1 - score, new_depth);
      else
        value = -black_all(1 - score, new_depth);
    }
  exit_loop:
    white_undo(move);
    count++;

    if (value >= score) {
      if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
        const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist + ((0xff00 - ist) * depth >> 8);

        if (move != position->killer_1) {
          position->killer_2 = position->killer_1;
          position->killer_1 = move;
        }
      }
      hash_low(position->hash_key, move, depth, value);
      return value;
    }

    if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
      const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];

      if (temp_position->score > score - 50)
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist - (ist * depth >> 8);
    }
  }

  if (!count && next->phase <= trans_value_2)
    return 0;
  value = score - 1;
  hash_high_cut(depth, value);
  return value;
}

int white_cut_check(const int score, const int depth) {
  int move, reduction, extend;
  int move_depth = 0, trans_move = 0, hash_score, new_depth, value, i;
  type_move_list list[256], *q;
  const uint64_t hash_key = position->hash_key;
  type_position* temp_position = position;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          if ((hash->flag & 8) != 8) {
            hash->age = age;
            return hash_score;
          }
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          hash->age = age;
          return hash_score;
        }
      }
    }
  }

  if (trans_move && !white_ok(trans_move))
    trans_move = 0;
  int best_value = temp_position - (root_position + 1) - 30000;
  int singular = 0;

  if (depth >= 16 && trans_move) {
    value = white_exclude_check(score - depth, depth - (12 <= depth / 2 ? 12 : depth / 2),
                                trans_move & 0x7fff);

    if (value < score - depth) {
      singular++;
      const int height = temp_position - (root_position + 1);

      if (height * 4 <= depth)
        singular++;
      value = white_exclude_check(score - 2 * depth,
                                  depth - (12 <= depth / 2 ? 12 : depth / 2), trans_move & 0x7fff);

      if (value < score - 2 * depth) {
        singular++;

        if (height * 8 <= depth)
          singular++;
      }
    }
  }
  type_move_list* p = list;
  list[0].move = trans_move;
  int count = 0;
  uint8_t gen = 0;
  list[1].move = 0;

  while (p->move || !gen) {
    if (!p->move) {
      type_move_list* move_list = white_evasion(list + 1, 0xffffffffffffffff);
      gen = 1;

      for (p = move_list - 1; p >= list + 1; p--) {
        if ((p->move & 0x7fff) == trans_move)
          p->move = 0;
        else if (p->move <= 0x80 << 24) {
          if ((p->move & 0x7fff) == temp_position->killer_1)
            p->move |= 0x7fff8000;

          else if ((p->move & 0x7fff) == temp_position->killer_2)
            p->move |= 0x7fff0000;

          else
            p->move |= (p->move & 0x7fff)
              | history_table[board.square[(p->move >> 6 & 077)]][(p->move & 077)]
              << 15;
        }
        move = p->move;

        for (q = p + 1; q < move_list; q++) {
          if (move < q->move)
            (q - 1)->move = q->move;
          else
            break;
        }
        q--;
        q->move = move;
      }
      p = list + 1;
      continue;
    }
    move = p->move & 0x7fff;
    p++;

    if (move != trans_move)
      singular = 0;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      count++;
      best_value = 0 >= best_value ? 0 : best_value;
      continue;
    }
    white_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check) {
      new_depth = depth - 2;

      if (singular)
        new_depth += singular;
      else
        new_depth++;

      if (new_depth <= 7)
        value = -black_low_check(1 - score, new_depth);
      else
        value = -black_all_check(1 - score, new_depth);
    }
    else {
      if (count >= 1) {
        if (depth > 8)
          reduction = bsr(depth - 7);
        else
          reduction = 0;
        reduction += 1 + (count <= 2 ? count : 2);

        if ((position->material & 0xff) >= 18)
          extend = 1;
        else
          extend = 0;
        new_depth = depth + extend - reduction - 2;

        if (new_depth <= 1)
          value = -black_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -black_low(1 - score, new_depth);

        else
          value = -black_all(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }

      if (!singular && (position->material & 0xff) >= 18)
        extend = 1;
      else
        extend = 0;
      new_depth = depth - 2 + extend + singular;

      if (new_depth <= 7)
        value = -black_low(1 - score, new_depth);
      else
        value = -black_all(1 - score, new_depth);
    }
  exit_loop:
    white_undo(move);

    if (value > best_value)
      best_value = value;

    if (value < score) {
      count++;
      continue;
    }
    hash_low(position->hash_key, move, 1 >= depth ? 1 : depth, value);
    return value;
  }
  hash_high_cut(1 >= depth ? 1 : depth, best_value);
  return best_value;
}

int black_all(const int score, const int depth) {
  int move, i;
  int move_depth = 0, trans_move = 0, hash_score;
  int value, new_depth, in_check;
  type_next next[1];
  type_position* temp_position = position;
  const uint64_t hash_key = position->hash_key;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          if (position->flag & 1 || move) {
            hash->age = age;
            return hash_score;
          }
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          if ((hash->flag & 4) != 4) {
            hash->age = age;
            return hash_score;
          }
        }
      }
    }
  }
  next->trans_move = trans_move;

  if (temp_position->score >= score && position->flag & 1) {
    do_null();
    new_depth = depth - 8;
    new_depth -=
      static_cast<uint32_t>((temp_position->score - score <= 96 ? temp_position->score - score : 96))
      / 32;

    if (new_depth <= 1)
      value = -white_qsearch(1 - score, 0);

    else if (new_depth <= 7)
      value = -white_low(1 - score, new_depth);

    else
      value = -white_cut(1 - score, new_depth);
    undo_null();

    if (value >= score) {
      if (trans_move == 0)
        hash_low_all(0, depth, value);
      return value;
    }
  }
  int count = 0;
  next->trans_move = trans_move;
  next->phase = trans_value;
  next->target = board.piece[occupied_white];

  if (depth < 20 && score - temp_position->score >= 48 * (depth - 4)) {
    next->phase = trans_value_2;
    count = 1;

    if (score - temp_position->score >= 48 * (depth - 2))
      next->target ^= board.piece[white_pawn];
  }
  next->move = 0;
  next->bad_capture = 0;

  while ((move = black_next(next))) {
    const int to = move & 077;
    const int from = move >> 6 & 077;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      count++;
      continue;
    }

    if (count > 5 && next->phase == ordinary_moves && (move & 0xe000) == 0
      && square_set[from] & ~position->black_xray && depth < 20) {
      if ((5 << (depth - 6))
        + static_cast<int>(max_increase[board.square[(move >> 6 & 077)]][move & 07777])
        + temp_position->score < score + 35 + 2 * count) {
        count++;
        continue;
      }
    }

    if (depth < 20 && (2 << (depth - 6)) + (temp_position->score) < score
      + 125 && next->phase == ordinary_moves && board.black_king != from
      && square_set[from] & ~position->black_xray && (move & 0x8000) == 0 && !black_see(move)) {
      count++;
      continue;
    }
    move &= 0x7fff;
    black_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check)
      in_check = 1;
    else
      in_check = 0;
    int extend = 0;

    if (board.square[to] == black_pawn && to <= h3
      && (board.piece[white_pawn] & passed_pawn_black[to]) == 0)
      extend = 1;

    if (in_check)
      value = -white_cut_check(1 - score, depth - 1);
    else {
      if (count > 5 && depth < 20 && (temp_position + 1)->capture == 0
        && (2 << (depth - 6)) - (temp_position + 1)->score < score + count - 15) {
        black_undo(move);
        count++;
        continue;
      }

      if (next->phase == ordinary_moves && count >= 3) {
        new_depth = depth - 2 + extend - bsr(1 + count);

        if (new_depth <= 1)
          value = -white_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -white_low(1 - score, new_depth);

        else
          value = -white_cut(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }
      new_depth = depth - 2 + extend;

      if (new_depth <= 7)
        value = -white_low(1 - score, new_depth);
      else
        value = -white_cut(1 - score, new_depth);
    }
  exit_loop:
    black_undo(move);
    count++;

    if (value >= score) {
      if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
        const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist + ((0xff00 - ist) * depth >> 8);

        if (move != position->killer_1) {
          position->killer_2 = position->killer_1;
          position->killer_1 = move;
        }
      }
      hash_low_all(move, depth, value);
      return value;
    }

    if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
      const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];

      if (temp_position->score > score - 50)
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist - (ist * depth >> 8);
    }
  }

  if (!count && next->phase <= trans_value_2)
    return 0;
  value = score - 1;
  hash_high(position->hash_key, depth, value);
  return value;
}

int black_all_check(const int score, const int depth) {
  int move, extend;
  int move_depth = 0, trans_move = 0, hash_score, new_depth, value, i;
  type_move_list list[256], *q;
  const uint64_t hash_key = position->hash_key;
  type_position* temp_position = position;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          hash->age = age;
          return hash_score;
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          if ((hash->flag & 4) != 4) {
            hash->age = age;
            return hash_score;
          }
        }
      }
    }
  }

  if (trans_move && !black_ok(trans_move))
    trans_move = 0;
  int best_value = temp_position - (root_position + 1) - 30000;
  type_move_list* p = list;
  list[0].move = trans_move;
  int count = 0;
  uint8_t gen = 0;
  list[1].move = 0;

  while (p->move || !gen) {
    if (!p->move) {
      type_move_list* move_list = black_evasion(list + 1, 0xffffffffffffffff);
      gen = 1;

      for (p = move_list - 1; p >= list + 1; p--) {
        if ((p->move & 0x7fff) == trans_move)
          p->move = 0;
        else if (p->move <= 0x80 << 24) {
          if ((p->move & 0x7fff) == temp_position->killer_1)
            p->move |= 0x7fff8000;

          else if ((p->move & 0x7fff) == temp_position->killer_2)
            p->move |= 0x7fff0000;

          else
            p->move |= (p->move & 0x7fff)
              | history_table[board.square[(p->move >> 6 & 077)]][(p->move & 077)]
              << 15;
        }
        move = p->move;

        for (q = p + 1; q < move_list; q++) {
          if (move < q->move)
            (q - 1)->move = q->move;
          else
            break;
        }
        q--;
        q->move = move;
      }
      p = list + 1;
      continue;
    }
    move = p->move & 0x7fff;
    p++;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      best_value = 0 >= best_value ? 0 : best_value;
      count++;
      continue;
    }
    black_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check) {
      new_depth = depth - 1;

      if (new_depth <= 7)
        value = -white_low_check(1 - score, new_depth);
      else
        value = -white_cut_check(1 - score, new_depth);
    }
    else {
      if (count >= 1) {
        if ((position->material & 0xff) >= 18)
          extend = 1;
        else
          extend = 0;
        new_depth = depth - 2 - (2 <= count ? 2 : count) + extend;

        if (new_depth <= 1)
          value = -white_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -white_low(1 - score, new_depth);

        else
          value = -white_cut(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }

      if ((position->material & 0xff) >= 18)
        extend = 1;
      else
        extend = 0;
      new_depth = depth - 2 + extend;

      if (new_depth <= 7)
        value = -white_low(1 - score, new_depth);
      else
        value = -white_cut(1 - score, new_depth);
    }
  exit_loop:
    black_undo(move);

    if (value > best_value)
      best_value = value;

    if (value < score) {
      count++;
      continue;
    }
    hash_low_all(move, 1 >= depth ? 1 : depth, value);
    return value;
  }
  hash_high(position->hash_key, 1 >= depth ? 1 : depth, best_value);
  return best_value;
}

int white_all(const int score, const int depth) {
  int move, i;
  int move_depth = 0, trans_move = 0, hash_score;
  int value, new_depth, in_check;
  type_next next[1];
  type_position* temp_position = position;
  const uint64_t hash_key = position->hash_key;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          if (position->flag & 2 || move) {
            hash->age = age;
            return hash_score;
          }
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          if ((hash->flag & 4) != 4) {
            hash->age = age;
            return hash_score;
          }
        }
      }
    }
  }
  next->trans_move = trans_move;

  if (temp_position->score >= score && position->flag & 2) {
    do_null();
    new_depth = depth - 8;
    new_depth -=
      static_cast<uint32_t>((temp_position->score - score <= 96 ? temp_position->score - score : 96))
      / 32;

    if (new_depth <= 1)
      value = -black_qsearch(1 - score, 0);

    else if (new_depth <= 7)
      value = -black_low(1 - score, new_depth);

    else
      value = -black_cut(1 - score, new_depth);
    undo_null();

    if (value >= score) {
      if (trans_move == 0)
        hash_low_all(0, depth, value);
      return value;
    }
  }
  int count = 0;
  next->trans_move = trans_move;
  next->phase = trans_value;
  next->target = board.piece[occupied_black];

  if (depth < 20 && score - temp_position->score >= 48 * (depth - 4)) {
    next->phase = trans_value_2;
    count = 1;

    if (score - temp_position->score >= 48 * (depth - 2))
      next->target ^= board.piece[black_pawn];
  }
  next->move = 0;
  next->bad_capture = 0;

  while ((move = white_next(next))) {
    const int to = move & 077;
    const int from = move >> 6 & 077;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      count++;
      continue;
    }

    if (count > 5 && next->phase == ordinary_moves && (move & 0xe000) == 0
      && square_set[from] & ~position->white_xray && depth < 20) {
      if ((5 << (depth - 6))
        + static_cast<int>(max_increase[board.square[(move >> 6 & 077)]][move & 07777])
        + temp_position->score < score + 35 + 2 * count) {
        count++;
        continue;
      }
    }

    if (depth < 20 && (2 << (depth - 6)) + (temp_position->score) < score
      + 125 && next->phase == ordinary_moves && board.white_king != from
      && square_set[from] & ~position->white_xray && (move & 0x8000) == 0 && !white_see(move)) {
      count++;
      continue;
    }
    move &= 0x7fff;
    white_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check)
      in_check = 1;
    else
      in_check = 0;
    int extend = 0;

    if (board.square[to] == white_pawn && to >= a6
      && (board.piece[black_pawn] & passed_pawn_white[to]) == 0)
      extend = 1;

    if (in_check)
      value = -black_cut_check(1 - score, depth - 1);
    else {
      if (count > 5 && depth < 20 && (temp_position + 1)->capture == 0
        && (2 << (depth - 6)) - (temp_position + 1)->score < score + count - 15) {
        white_undo(move);
        count++;
        continue;
      }

      if (next->phase == ordinary_moves && count >= 3) {
        new_depth = depth - 2 + extend - bsr(1 + count);

        if (new_depth <= 1)
          value = -black_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -black_low(1 - score, new_depth);

        else
          value = -black_cut(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }
      new_depth = depth - 2 + extend;

      if (new_depth <= 7)
        value = -black_low(1 - score, new_depth);
      else
        value = -black_cut(1 - score, new_depth);
    }
  exit_loop:
    white_undo(move);
    count++;

    if (value >= score) {
      if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
        const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist + ((0xff00 - ist) * depth >> 8);

        if (move != position->killer_1) {
          position->killer_2 = position->killer_1;
          position->killer_1 = move;
        }
      }
      hash_low_all(move, depth, value);
      return value;
    }

    if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
      const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];

      if (temp_position->score > score - 50)
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist - (ist * depth >> 8);
    }
  }

  if (!count && next->phase <= trans_value_2)
    return 0;
  value = score - 1;
  hash_high(position->hash_key, depth, value);
  return value;
}

int white_all_check(const int score, const int depth) {
  int move, extend;
  int move_depth = 0, trans_move = 0, hash_score, new_depth, value, i;
  type_move_list list[256], *q;
  const uint64_t hash_key = position->hash_key;
  type_position* temp_position = position;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          hash->age = age;
          return hash_score;
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          if ((hash->flag & 4) != 4) {
            hash->age = age;
            return hash_score;
          }
        }
      }
    }
  }

  if (trans_move && !white_ok(trans_move))
    trans_move = 0;
  int best_value = temp_position - (root_position + 1) - 30000;
  type_move_list* p = list;
  list[0].move = trans_move;
  int count = 0;
  uint8_t gen = 0;
  list[1].move = 0;

  while (p->move || !gen) {
    if (!p->move) {
      type_move_list* move_list = white_evasion(list + 1, 0xffffffffffffffff);
      gen = 1;

      for (p = move_list - 1; p >= list + 1; p--) {
        if ((p->move & 0x7fff) == trans_move)
          p->move = 0;
        else if (p->move <= 0x80 << 24) {
          if ((p->move & 0x7fff) == temp_position->killer_1)
            p->move |= 0x7fff8000;

          else if ((p->move & 0x7fff) == temp_position->killer_2)
            p->move |= 0x7fff0000;

          else
            p->move |= (p->move & 0x7fff)
              | history_table[board.square[(p->move >> 6 & 077)]][(p->move & 077)]
              << 15;
        }
        move = p->move;

        for (q = p + 1; q < move_list; q++) {
          if (move < q->move)
            (q - 1)->move = q->move;
          else
            break;
        }
        q--;
        q->move = move;
      }
      p = list + 1;
      continue;
    }
    move = p->move & 0x7fff;
    p++;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      best_value = 0 >= best_value ? 0 : best_value;
      count++;
      continue;
    }
    white_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check) {
      new_depth = depth - 1;

      if (new_depth <= 7)
        value = -black_low_check(1 - score, new_depth);
      else
        value = -black_cut_check(1 - score, new_depth);
    }
    else {
      if (count >= 1) {
        if ((position->material & 0xff) >= 18)
          extend = 1;
        else
          extend = 0;
        new_depth = depth - 2 - (2 <= count ? 2 : count) + extend;

        if (new_depth <= 1)
          value = -black_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -black_low(1 - score, new_depth);

        else
          value = -black_cut(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }

      if ((position->material & 0xff) >= 18)
        extend = 1;
      else
        extend = 0;
      new_depth = depth - 2 + extend;

      if (new_depth <= 7)
        value = -black_low(1 - score, new_depth);
      else
        value = -black_cut(1 - score, new_depth);
    }
  exit_loop:
    white_undo(move);

    if (value > best_value)
      best_value = value;

    if (value < score) {
      count++;
      continue;
    }
    hash_low_all(move, 1 >= depth ? 1 : depth, value);
    return value;
  }
  hash_high(position->hash_key, 1 >= depth ? 1 : depth, best_value);
  return best_value;
}

int black_exclude(const int score, const int depth, const uint32_t mv) {
  int move, i;
  int move_depth = 0, trans_move = 0, hash_score;
  int value, new_depth, in_check;
  type_next next[1];
  type_position* temp_position = position;
  uint64_t hash_key = position->hash_key;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  hash_key ^=
    rand_hash_table[black_king][(mv >> 6 & 077)] ^ rand_hash_table[white_king][(mv & 077)];
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          if (position->flag & 1 || move) {
            hash->age = age;
            return hash_score;
          }
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          if (true) {
            hash->age = age;
            return hash_score;
          }
        }
      }
    }
  }
  next->trans_move = trans_move;

  if (temp_position->score >= score && position->flag & 1) {
    do_null();
    new_depth = depth - 8;
    new_depth -=
      static_cast<uint32_t>((temp_position->score - score <= 96 ? temp_position->score - score : 96))
      / 32;

    if (new_depth <= 1)
      value = -white_qsearch(1 - score, 0);

    else if (new_depth <= 7)
      value = -white_low(1 - score, new_depth);

    else
      value = -white_cut(1 - score, new_depth);
    undo_null();

    if (value >= score) {
      if (trans_move == 0)
        hash_low(hash_key, 0, depth, value);
      return value;
    }
  }
  int count = 0;
  next->trans_move = trans_move;
  next->phase = trans_value;
  next->target = board.piece[occupied_white];

  if (depth < 20 && score - temp_position->score >= 48 * (depth - 4)) {
    next->phase = trans_value_2;
    count = 1;

    if (score - temp_position->score >= 48 * (depth - 2))
      next->target ^= board.piece[white_pawn];
  }
  next->move = 0;
  next->bad_capture = 0;

  while ((move = black_next(next))) {
    if ((move & 0x7fff) == (mv & 0x7fff))
      continue;
    const int to = move & 077;
    const int from = move >> 6 & 077;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      count++;
      continue;
    }

    if (count > 5 && next->phase == ordinary_moves && (move & 0xe000) == 0
      && square_set[from] & ~position->black_xray && depth < 20) {
      if ((6 << (depth - 6))
        + static_cast<int>(max_increase[board.square[(move >> 6 & 077)]][move & 07777])
        + temp_position->score < score + 30 + 2 * count) {
        count++;
        continue;
      }
    }

    if (depth < 20 && (2 << (depth - 6)) + (temp_position->score) < score
      + 125 && next->phase == ordinary_moves && board.black_king != from
      && square_set[from] & ~position->black_xray && (move & 0x8000) == 0 && !black_see(move)) {
      count++;
      continue;
    }
    move &= 0x7fff;
    black_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check)
      in_check = 1;
    else
      in_check = 0;
    int extend = 0;

    if (board.square[to] == black_pawn && to <= h3
      && (board.piece[white_pawn] & passed_pawn_black[to]) == 0)
      extend = 1;

    if (in_check)
      value = -white_cut_check(1 - score, depth - 1);
    else {
      if (count > 5 && depth < 20 && (temp_position + 1)->capture == 0
        && (2 << (depth - 6)) - (temp_position + 1)->score < score + count - 15) {
        black_undo(move);
        count++;
        continue;
      }

      if (next->phase == ordinary_moves && (count >= 3)) {
        constexpr int reduction = 0;
        new_depth = depth - 2 + extend - bsr(1 + count) - reduction;

        if (new_depth <= 1)
          value = -white_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -white_low(1 - score, new_depth);

        else
          value = -white_cut(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }
      new_depth = depth - 2 + extend;

      if (new_depth <= 7)
        value = -white_low(1 - score, new_depth);
      else
        value = -white_cut(1 - score, new_depth);
    }
  exit_loop:
    black_undo(move);
    count++;

    if (value >= score) {
      if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
        const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist + ((0xff00 - ist) * depth >> 8);

        if (move != position->killer_1) {
          position->killer_2 = position->killer_1;
          position->killer_1 = move;
        }
      }
      hash_low(hash_key, move, depth, value);
      return value;
    }

    if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
      const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];

      if (temp_position->score > score - 50)
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist - (ist * depth >> 8);
    }
  }
  value = score - 1;
  hash_high(hash_key, depth, value);
  return value;
}

int black_exclude_check(const int score, const int depth, const uint32_t mv) {
  int move, extend;
  int move_depth = 0, trans_move = 0, hash_score, new_depth, value, i;
  type_move_list list[256], *q;
  uint64_t hash_key = position->hash_key;
  type_position* temp_position = position;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  hash_key ^=
    rand_hash_table[black_king][(mv >> 6 & 077)] ^ rand_hash_table[white_king][(mv & 077)];
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          if (true) {
            hash->age = age;
            return hash_score;
          }
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          if (true) {
            hash->age = age;
            return hash_score;
          }
        }
      }
    }
  }

  if (trans_move && !black_ok(trans_move))
    trans_move = 0;
  int best_value = temp_position - (root_position + 1) - 30000;
  type_move_list* p = list;
  list[0].move = trans_move;
  int count = 0;
  uint8_t gen = 0;
  list[1].move = 0;

  while (p->move || !gen) {
    if (!p->move) {
      type_move_list* move_list = black_evasion(list + 1, 0xffffffffffffffff);
      gen = 1;

      for (p = move_list - 1; p >= list + 1; p--) {
        if ((p->move & 0x7fff) == trans_move)
          p->move = 0;
        else if (p->move <= 0x80 << 24) {
          if ((p->move & 0x7fff) == temp_position->killer_1)
            p->move |= 0x7fff8000;

          else if ((p->move & 0x7fff) == temp_position->killer_2)
            p->move |= 0x7fff0000;

          else
            p->move |= (p->move & 0x7fff)
              | history_table[board.square[(p->move >> 6 & 077)]][(p->move & 077)]
              << 15;
        }
        move = p->move;

        for (q = p + 1; q < move_list; q++) {
          if (move < q->move)
            (q - 1)->move = q->move;
          else
            break;
        }
        q--;
        q->move = move;
      }
      p = list + 1;
      continue;
    }
    move = p->move & 0x7fff;
    p++;

    if (move == mv)
      continue;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      best_value = 0 >= best_value ? 0 : best_value;
      count++;
      continue;
    }
    black_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check) {
      new_depth = depth - 1;

      if (new_depth <= 7)
        value = -white_low_check(1 - score, new_depth);
      else
        value = -white_cut_check(1 - score, new_depth);
    }
    else {
      if (count >= 1) {
        if ((position->material & 0xff) >= 18)
          extend = 1;
        else
          extend = 0;
        new_depth = depth - 2 - (2 <= count ? 2 : count) + extend;

        if (new_depth <= 1)
          value = -white_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -white_low(1 - score, new_depth);

        else
          value = -white_cut(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }

      if ((position->material & 0xff) >= 18)
        extend = 1;
      else
        extend = 0;
      new_depth = depth - 2 + extend;

      if (new_depth <= 7)
        value = -white_low(1 - score, new_depth);
      else
        value = -white_cut(1 - score, new_depth);
    }
  exit_loop:
    black_undo(move);

    if (value > best_value)
      best_value = value;

    if (value < score) {
      count++;
      continue;
    }
    hash_low(hash_key, move, 1 >= depth ? 1 : depth, value);
    return value;
  }
  hash_high(hash_key, 1 >= depth ? 1 : depth, best_value);
  return best_value;
}

int white_exclude(const int score, const int depth, const uint32_t mv) {
  int move, i;
  int move_depth = 0, trans_move = 0, hash_score;
  int value, new_depth, in_check;
  type_next next[1];
  type_position* temp_position = position;
  uint64_t hash_key = position->hash_key;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  hash_key ^=
    rand_hash_table[white_king][(mv >> 6 & 077)] ^ rand_hash_table[black_king][(mv & 077)];
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          if (position->flag & 2 || move) {
            hash->age = age;
            return hash_score;
          }
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          if (true) {
            hash->age = age;
            return hash_score;
          }
        }
      }
    }
  }
  next->trans_move = trans_move;

  if (temp_position->score >= score && position->flag & 2) {
    do_null();
    new_depth = depth - 8;
    new_depth -=
      static_cast<uint32_t>((temp_position->score - score <= 96 ? temp_position->score - score : 96))
      / 32;

    if (new_depth <= 1)
      value = -black_qsearch(1 - score, 0);

    else if (new_depth <= 7)
      value = -black_low(1 - score, new_depth);

    else
      value = -black_cut(1 - score, new_depth);
    undo_null();

    if (value >= score) {
      if (trans_move == 0)
        hash_low(hash_key, 0, depth, value);
      return value;
    }
  }
  int count = 0;
  next->trans_move = trans_move;
  next->phase = trans_value;
  next->target = board.piece[occupied_black];

  if (depth < 20 && score - temp_position->score >= 48 * (depth - 4)) {
    next->phase = trans_value_2;
    count = 1;

    if (score - temp_position->score >= 48 * (depth - 2))
      next->target ^= board.piece[black_pawn];
  }

  next->move = 0;
  next->bad_capture = 0;

  while ((move = white_next(next))) {
    if ((move & 0x7fff) == (mv & 0x7fff))
      continue;
    const int to = move & 077;
    const int from = move >> 6 & 077;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      count++;
      continue;
    }

    if (count > 5 && next->phase == ordinary_moves && (move & 0xe000) == 0
      && square_set[from] & ~position->white_xray && depth < 20) {
      if ((6 << (depth - 6))
        + static_cast<int>(max_increase[board.square[(move >> 6 & 077)]][move & 07777])
        + temp_position->score < score + 30 + 2 * count) {
        count++;
        continue;
      }
    }

    if (depth < 20 && (2 << (depth - 6)) + (temp_position->score) < score
      + 125 && next->phase == ordinary_moves && board.white_king != from
      && square_set[from] & ~position->white_xray && (move & 0x8000) == 0 && !white_see(move)) {
      count++;
      continue;
    }
    move &= 0x7fff;
    white_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check)
      in_check = 1;
    else
      in_check = 0;
    int extend = 0;

    if (board.square[to] == white_pawn && to >= a6
      && (board.piece[black_pawn] & passed_pawn_white[to]) == 0)
      extend = 1;

    if (in_check)
      value = -black_cut_check(1 - score, depth - 1);
    else {
      if (count > 5 && depth < 20 && (temp_position + 1)->capture == 0
        && (2 << (depth - 6)) - (temp_position + 1)->score < score + count - 15) {
        white_undo(move);
        count++;
        continue;
      }

      if (next->phase == ordinary_moves && (count >= 3)) {
        constexpr int reduction = 0;
        new_depth = depth - 2 + extend - bsr(1 + count) - reduction;

        if (new_depth <= 1)
          value = -black_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -black_low(1 - score, new_depth);

        else
          value = -black_cut(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }
      new_depth = depth - 2 + extend;

      if (new_depth <= 7)
        value = -black_low(1 - score, new_depth);
      else
        value = -black_cut(1 - score, new_depth);
    }
  exit_loop:
    white_undo(move);
    count++;

    if (value >= score) {
      if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
        const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist + ((0xff00 - ist) * depth >> 8);

        if (move != position->killer_1) {
          position->killer_2 = position->killer_1;
          position->killer_1 = move;
        }
      }
      hash_low(hash_key, move, depth, value);
      return value;
    }

    if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
      const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];

      if (temp_position->score > score - 50)
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist - (ist * depth >> 8);
    }
  }
  value = score - 1;
  hash_high(hash_key, depth, value);
  return value;
}

int white_exclude_check(const int score, const int depth, const uint32_t mv) {
  int move, extend;
  int move_depth = 0, trans_move = 0, hash_score, new_depth, value, i;
  type_move_list list[256], *q;
  uint64_t hash_key = position->hash_key;
  type_position* temp_position = position;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  hash_key ^=
    rand_hash_table[white_king][(mv >> 6 & 077)] ^ rand_hash_table[black_king][(mv & 077)];
  (temp_position + 1)->move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (type_hash* hash = hash_table + (k + i); (hash->hash_key ^ hash_key >> 32) == 0) {
      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        (temp_position + 1)->move = trans_move = move;
      }

      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          if (true) {
            hash->age = age;
            return hash_score;
          }
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score) {
          if (true) {
            hash->age = age;
            return hash_score;
          }
        }
      }
    }
  }

  if (trans_move && !white_ok(trans_move))
    trans_move = 0;
  int best_value = temp_position - (root_position + 1) - 30000;
  type_move_list* p = list;
  list[0].move = trans_move;
  int count = 0;
  uint8_t gen = 0;
  list[1].move = 0;

  while (p->move || !gen) {
    if (!p->move) {
      type_move_list* move_list = white_evasion(list + 1, 0xffffffffffffffff);
      gen = 1;

      for (p = move_list - 1; p >= list + 1; p--) {
        if ((p->move & 0x7fff) == trans_move)
          p->move = 0;
        else if (p->move <= 0x80 << 24) {
          if ((p->move & 0x7fff) == temp_position->killer_1)
            p->move |= 0x7fff8000;

          else if ((p->move & 0x7fff) == temp_position->killer_2)
            p->move |= 0x7fff0000;

          else
            p->move |= (p->move & 0x7fff)
              | history_table[board.square[(p->move >> 6 & 077)]][(p->move & 077)]
              << 15;
        }
        move = p->move;

        for (q = p + 1; q < move_list; q++) {
          if (move < q->move)
            (q - 1)->move = q->move;
          else
            break;
        }
        q--;
        q->move = move;
      }
      p = list + 1;
      continue;
    }
    move = p->move & 0x7fff;
    p++;

    if (move == mv)
      continue;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      best_value = 0 >= best_value ? 0 : best_value;
      count++;
      continue;
    }
    white_make(move);
    eval(score - 300, score + 300, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check) {
      new_depth = depth - 1;

      if (new_depth <= 7)
        value = -black_low_check(1 - score, new_depth);
      else
        value = -black_cut_check(1 - score, new_depth);
    }
    else {
      if (count >= 1) {
        if ((position->material & 0xff) >= 18)
          extend = 1;
        else
          extend = 0;
        new_depth = depth - 2 - (2 <= count ? 2 : count) + extend;

        if (new_depth <= 1)
          value = -black_qsearch(1 - score, 0);

        else if (new_depth <= 7)
          value = -black_low(1 - score, new_depth);

        else
          value = -black_cut(1 - score, new_depth);

        if (value < score)
          goto exit_loop;
      }

      if ((position->material & 0xff) >= 18)
        extend = 1;
      else
        extend = 0;
      new_depth = depth - 2 + extend;

      if (new_depth <= 7)
        value = -black_low(1 - score, new_depth);
      else
        value = -black_cut(1 - score, new_depth);
    }
  exit_loop:
    white_undo(move);

    if (value > best_value)
      best_value = value;

    if (value < score) {
      count++;
      continue;
    }
    hash_low(hash_key, move, 1 >= depth ? 1 : depth, value);
    return value;
  }
  hash_high(hash_key, 1 >= depth ? 1 : depth, best_value);
  return best_value;
}

int black_low(const int score, const int depth) {
  int hash_score, i, trans_move = 0, move, move_depth = 0;
  type_next next[1];
  type_position* temp_position = position;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;
  int value = temp_position->score + 1125;

  if (value < score)
    return score - 1;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = position->hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          (temp_position + 1)->move = hash->move;
          return hash_score;
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score)
          return hash_score;
      }

      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }
  value = temp_position->score - (70 + 10 * depth);

  if (value >= score)
    return temp_position->score;
  int best_value = temp_position->score <= score - 1 ? temp_position->score : score - 1;

  if (temp_position->score >= score && position->flag & 1) {
    do_null();
    value = -white_qsearch(1 - score, 0);
    undo_null();

    if (value >= score) {
      hash_low(position->hash_key, trans_move, depth, value);
      return value;
    }
  }
  next->phase = trans_value;
  next->target = board.piece[occupied_white];

  if (temp_position->score + 50 + 8 * depth < score) {
    next->phase = trans_value_2;

    if (score >= temp_position->score + 75 + 32 * depth) {
      next->target ^= board.piece[white_pawn];

      if (board.piece[white_pawn] & position->black_attack)
        best_value += 125;

      if (depth <= 3 && score >= temp_position->score + 400 + 32 * depth) {
        next->target ^= board.piece[white_knight] | (board.piece[white_king_bishop] | board.piece[
          white_queen_bishop]);
        best_value += 300;

        if (score >= temp_position->score + 600 + 32 * depth) {
          next->target ^= board.piece[white_rook];
          best_value += 200;
        }
      }
    }
  }
  else if (depth <= 3 && temp_position->score + 4 * depth < score) {
    next->phase = trans_value_3;
    next->mask = score - temp_position->score + 4 * depth + 5;
  }
  next->bad_capture = 0;
  next->move = 0;
  next->trans_move = trans_move;
  int count = 0;

  while ((move = black_next(next))) {
    const int to = move & 077;
    const int from = move >> 6 & 077;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      best_value = 0 >= best_value ? 0 : best_value;
      count++;
      continue;
    }

    if (count >= depth && next->phase == ordinary_moves && board.piece[occupied_black]
      ^ (board.piece[black_king] | board.piece[black_pawn])
      && (move & 0xe000) == 0 && square_set[from] & ~position->black_xray) {
      if (2 * depth + static_cast<int>(max_increase[board.square[(move >> 6 & 077)]][move & 07777])
        + temp_position->score < score + 40 + 2 * count) {
        count++;
        continue;
      }
    }

    if ((board.square[to] == 0 || (depth <= 5 && !(move & 0x300000)))
      && square_set[from] & ~position->black_xray
      && board.square[from] != black_king && (move & 070000) != 030000
      && move != trans_move && !black_see(move)) {
      count++;
      continue;
    }
    move &= 0x7fff;
    black_make(move);
    eval(score - 150, score + 150, move);

    if ((temp_position + 1)->black_king_check
      || (next->phase == phase && (temp_position + 1)->white_king_check)) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check)
      value = -white_low_check(1 - score, depth - 1);
    else {
      if (count >= depth && 2 * depth - (temp_position + 1)->score < score + count) {
        black_undo(move);
        count++;
        continue;
      }

      if (depth <= 3)
        value = -white_qsearch(1 - score, 0);
      else
        value = -white_low(1 - score, depth - 2);
    }
    count++;
    black_undo(move);

    if (value >= score) {
      if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
        const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist + ((0xff00 - ist) * depth >> 8);

        if (move != position->killer_1) {
          position->killer_2 = position->killer_1;
          position->killer_1 = move;
        }
      }
      hash_low(position->hash_key, move, depth, value);
      return value;
    }

    if (value >= best_value)
      best_value = value;

    if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
      const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];

      if (temp_position->score > score - 50)
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist - (ist * depth >> 8);
    }
  }

  if (!count && next->phase <= trans_value_2)
    return 0;
  hash_high(position->hash_key, depth, best_value);
  return best_value;
}

int black_low_check(const int score, const int depth) {
  int trans_move = 0, move_depth = 0, hash_score, i, move,
      value;
  type_move_list list[256], *q;
  const type_position* temp_position = position;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  const int k = position->hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->depth_low && hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score)
          return hash_score;
      }

      if (hash->depth_high && hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score)
          return hash_score;
      }

      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }

  if (trans_move && !black_ok(trans_move))
    trans_move = 0;
  int best_value = temp_position - (root_position + 1) - 30000;
  type_move_list* p = list;
  list[0].move = trans_move;
  uint8_t gen = 0;
  list[1].move = 0;
  int count = 0;

  while (p->move || !gen) {
    if (!p->move) {
      type_move_list* move_list = black_evasion(list + 1, 0xffffffffffffffff);
      gen = 1;

      for (p = move_list - 1; p >= list + 1; p--) {
        if ((p->move & 0x7fff) == trans_move)
          p->move = 0;
        else if (p->move <= 0x80 << 24) {
          if ((p->move & 0x7fff) == temp_position->killer_1)
            p->move |= 0x7fff8000;

          else if ((p->move & 0x7fff) == temp_position->killer_2)
            p->move |= 0x7fff0000;

          else
            p->move |= (p->move & 0xffff)
              | history_table[board.square[(p->move >> 6 & 077)]][(p->move & 077)] >> 1
              << 16;
        }
        move = p->move;

        for (q = p + 1; q < move_list; q++) {
          if (move < q->move)
            (q - 1)->move = q->move;
          else
            break;
        }
        q--;
        q->move = move;
      }
      p = list + 1;
      continue;
    }
    move = p->move;
    p++;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      best_value = 0 >= best_value ? 0 : best_value;
      continue;
    }

    if (move & 1 << 15 && score > -25000 && (move & 0x7fff) != trans_move && !black_see(move)) {
      count++;
      continue;
    }
    move &= 0x7fff;
    black_make(move);
    eval(score - 150, score + 150, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check)
      value = -white_low_check(1 - score, depth - 1 + ((position->material & 0xff) >= 18));
    else {
      if (const int new_depth = depth - 2 + ((position->material & 0xff) >= 18); new_depth <= 1)
        value = -white_qsearch(1 - score, 0);
      else
        value = -white_low(1 - score, new_depth);
    }
    black_undo(move);

    if (value <= best_value)
      continue;
    best_value = value;

    if (value >= score) {
      hash_low(position->hash_key, move, 1 >= depth ? 1 : depth, value);
      return value;
    }
  }

  if (count && best_value < -25000)
    best_value = score - 1;
  hash_high(position->hash_key, 1 >= depth ? 1 : depth, best_value);
  return best_value;
}

int white_low(const int score, const int depth) {
  int hash_score, i, trans_move = 0, move, move_depth = 0;
  type_next next[1];
  type_position* temp_position = position;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  (temp_position + 1)->move = 0;
  int value = temp_position->score + 1125;

  if (value < score)
    return score - 1;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const int k = position->hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score) {
          (temp_position + 1)->move = hash->move;
          return hash_score;
        }
      }

      if (hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score)
          return hash_score;
      }

      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }
  value = temp_position->score - (70 + 10 * depth);

  if (value >= score)
    return temp_position->score;
  int best_value = temp_position->score <= score - 1 ? temp_position->score : score - 1;

  if (temp_position->score >= score && position->flag & 2) {
    do_null();
    value = -black_qsearch(1 - score, 0);
    undo_null();

    if (value >= score) {
      hash_low(position->hash_key, trans_move, depth, value);
      return value;
    }
  }
  next->phase = trans_value;
  next->target = board.piece[occupied_black];

  if (temp_position->score + 50 + 8 * depth < score) {
    next->phase = trans_value_2;

    if (score >= temp_position->score + 75 + 32 * depth) {
      next->target ^= board.piece[black_pawn];

      if (board.piece[black_pawn] & position->white_attack)
        best_value += 125;

      if (depth <= 3 && score >= temp_position->score + 400 + 32 * depth) {
        next->target ^= board.piece[black_knight] | (board.piece[black_queen_bishop] | board.piece[
          black_king_bishop]);
        best_value += 300;

        if (score >= temp_position->score + 600 + 32 * depth) {
          next->target ^= board.piece[black_rook];
          best_value += 200;
        }
      }
    }
  }
  else if (depth <= 3 && temp_position->score + 4 * depth < score) {
    next->phase = trans_value_3;
    next->mask = score - temp_position->score + 4 * depth + 5;
  }
  next->bad_capture = 0;
  next->move = 0;
  next->trans_move = trans_move;
  int count = 0;

  while ((move = white_next(next))) {
    const int to = move & 077;
    const int from = move >> 6 & 077;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      best_value = 0 >= best_value ? 0 : best_value;
      count++;
      continue;
    }

    if (count >= depth && next->phase == ordinary_moves && board.piece[occupied_white]
      ^ (board.piece[white_king] | board.piece[white_pawn])
      && (move & 0xe000) == 0 && square_set[from] & ~position->white_xray) {
      if (2 * depth + static_cast<int>(max_increase[board.square[(move >> 6 & 077)]][move & 07777])
        + temp_position->score < score + 40 + 2 * count) {
        count++;
        continue;
      }
    }

    if ((board.square[to] == 0 || (depth <= 5 && !(move & 0x300000)))
      && square_set[from] & ~position->white_xray
      && board.square[from] != white_king && (move & 070000) != 030000
      && move != trans_move && !white_see(move)) {
      count++;
      continue;
    }
    move &= 0x7fff;
    white_make(move);
    eval(score - 150, score + 150, move);

    if ((temp_position + 1)->white_king_check
      || (next->phase == phase && (temp_position + 1)->black_king_check)) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check)
      value = -black_low_check(1 - score, depth - 1);
    else {
      if (count >= depth && 2 * depth - (temp_position + 1)->score < score + count) {
        white_undo(move);
        count++;
        continue;
      }

      if (depth <= 3)
        value = -black_qsearch(1 - score, 0);
      else
        value = -black_low(1 - score, depth - 2);
    }
    count++;
    white_undo(move);

    if (value >= score) {
      if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
        const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist + ((0xff00 - ist) * depth >> 8);

        if (move != position->killer_1) {
          position->killer_2 = position->killer_1;
          position->killer_1 = move;
        }
      }
      hash_low(position->hash_key, move, depth, value);
      return value;
    }

    if (value >= best_value)
      best_value = value;

    if ((temp_position + 1)->capture == 0 && (move & 060000) == 0) {
      const int ist = history_table[board.square[(move >> 6 & 077)]][(move & 077)];

      if (temp_position->score > score - 50)
        history_table[board.square[(move >> 6 & 077)]][(move & 077)] =
          ist - (ist * depth >> 8);
    }
  }

  if (!count && next->phase <= trans_value_2)
    return 0;
  hash_high(position->hash_key, depth, best_value);
  return best_value;
}

int white_low_check(const int score, const int depth) {
  int trans_move = 0, move_depth = 0, hash_score, i, move,
      value;
  type_move_list list[256], *q;
  const type_position* temp_position = position;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;
  const int k = position->hash_key & hash_mask;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->depth_low && hash->depth_low >= depth) {
        hash_score = hash->score_low;

        if (hash_score >= score)
          return hash_score;
      }

      if (hash->depth_high && hash->depth_high >= depth) {
        hash_score = hash->score_high;

        if (hash_score < score)
          return hash_score;
      }

      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }

  if (trans_move && !white_ok(trans_move))
    trans_move = 0;
  int best_value = temp_position - (root_position + 1) - 30000;
  type_move_list* p = list;
  list[0].move = trans_move;
  uint8_t gen = 0;
  list[1].move = 0;
  int count = 0;

  while (p->move || !gen) {
    if (!p->move) {
      type_move_list* move_list = white_evasion(list + 1, 0xffffffffffffffff);
      gen = 1;

      for (p = move_list - 1; p >= list + 1; p--) {
        if ((p->move & 0x7fff) == trans_move)
          p->move = 0;
        else if (p->move <= 0x80 << 24) {
          if ((p->move & 0x7fff) == temp_position->killer_1)
            p->move |= 0x7fff8000;

          else if ((p->move & 0x7fff) == temp_position->killer_2)
            p->move |= 0x7fff0000;

          else
            p->move |= (p->move & 0xffff)
              | history_table[board.square[(p->move >> 6 & 077)]][(p->move & 077)] >> 1
              << 16;
        }
        move = p->move;

        for (q = p + 1; q < move_list; q++) {
          if (move < q->move)
            (q - 1)->move = q->move;
          else
            break;
        }
        q--;
        q->move = move;
      }
      p = list + 1;
      continue;
    }
    move = p->move;
    p++;

    if (score > 0 && temp_position->reversible >= 2
      && ((((move) & 077) << 6) | (((move) >> 6) & 077)) == (temp_position - 1)->move
      && board.square[(move & 077)] == 0) {
      best_value = 0 >= best_value ? 0 : best_value;
      continue;
    }

    if (move & 1 << 15 && score > -25000 && (move & 0x7fff) != trans_move && !white_see(move)) {
      count++;
      continue;
    }
    move &= 0x7fff;
    white_make(move);
    eval(score - 150, score + 150, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check)
      value = -black_low_check(1 - score, depth - 1 + ((position->material & 0xff) >= 18));
    else {
      if (const int new_depth = depth - 2 + ((position->material & 0xff) >= 18); new_depth <= 1)
        value = -black_qsearch(1 - score, 0);
      else
        value = -black_low(1 - score, new_depth);
    }
    white_undo(move);

    if (value <= best_value)
      continue;
    best_value = value;

    if (value >= score) {
      hash_low(position->hash_key, move, 1 >= depth ? 1 : depth, value);
      return value;
    }
  }

  if (count && best_value < -25000)
    best_value = score - 1;
  hash_high(position->hash_key, 1 >= depth ? 1 : depth, best_value);
  return best_value;
}

int black_qsearch(const int score, const int depth) {
  int hash_score, i;
  const int k = position->hash_key & hash_mask;
  uint32_t move, trans_move = 0, move_depth = 0;
  type_move_list list[256];
  const type_position* temp_position = position;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->depth_low) {
        hash_score = hash->score_low;

        if (hash_score >= score)
          return hash_score;
      }

      if (hash->depth_high) {
        hash_score = hash->score_high;

        if (hash_score < score)
          return hash_score;
      }

      const uint32_t trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }
  int best_value = temp_position->score + 5;

  if (best_value >= score)
    return best_value;
  int value = score - 160;
  uint64_t target = board.piece[occupied_white];

  if (value > best_value) {
    value = score - 500;
    target ^= board.piece[white_pawn];

    if (value > best_value) {
      target ^= board.piece[white_knight] | (board.piece[white_king_bishop] | board.piece[white_queen_bishop]);
      value = score - 800;

      if (value > best_value)
        target ^= board.piece[white_rook];
    }

    if (board.piece[white_pawn] & position->black_attack)
      best_value += 160;
  }
  const type_move_list* move_list = black_capture(list, target);
  type_move_list* p = list;

  while (p->move) {
    if ((p->move & 0x7fff) == trans_move)
      p->move |= 0xffff0000;
    p++;
  }
  p = list;

  while (p->move) {
    move = p->move;
    type_move_list* q = ++p;

    while (q->move) {
      if (move < q->move) {
        const uint32_t temp = q->move;
        q->move = move;
        move = temp;
      }
      q++;
    }

    if (!(move & 0x300000) && (move & 0x7fff) != trans_move
      && square_set[(move >> 6 & 077)] & ~position->black_xray && !black_see(move))
      continue;
    move &= 0x7fff;
    black_make(move);
    eval(score - 150, score + 150, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check)
      value = -white_qsearch_check(1 - score, depth - 1);
    else
      value = -white_qsearch(1 - score, depth - 1);
    black_undo(move);

    if (value <= best_value)
      continue;
    best_value = value;

    if (value >= score) {
      hash_low(position->hash_key, move, 1, value);
      return value;
    }
  }

  if (depth >= -1 && temp_position->score >= score - (100 + (12 << (depth + 4)))) {
    move_list = black_check(list, target);

    for (i = 0; i < move_list - list; i++) {
      move = list[i].move;
      move &= 0x7fff;
      black_make(move);
      eval(score - 150, score + 150, move);

      if ((temp_position + 1)->black_king_check) {
        black_undo(move);
        continue;
      }
      value = -white_qsearch_check(1 - score, depth - 1);
      black_undo(move);

      if (value <= best_value)
        continue;
      best_value = value;

      if (value >= score) {
        hash_low(position->hash_key, move, 1, value);
        return value;
      }
    }
  }
  hash_high(position->hash_key, 1, best_value);
  return best_value;
}

int black_qsearch_check(const int score, int depth) {
  int hash_score, i, value,
      move_depth = 0;
  const int k = position->hash_key & hash_mask;
  type_move_list list[256];
  uint32_t move, trans_move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const type_position* temp_position = position;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->depth_low) {
        hash_score = hash->score_low;

        if (hash_score >= score)
          return hash_score;
      }

      if (hash->depth_high) {
        hash_score = hash->score_high;

        if (hash_score < score)
          return hash_score;
      }

      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }
  int best_value = temp_position - (root_position + 1) - 30000;
  uint64_t target = 0xffffffffffffffff;

  if (temp_position->score + 10 < score) {
    best_value = temp_position->score + 10;
    value = score - 200;
    target = board.piece[occupied_white];

    if (value > best_value) {
      target ^= board.piece[white_pawn];
      value = score - 500;
      best_value += 200;

      if (value > best_value)
        target ^= board.piece[white_knight] | (board.piece[white_king_bishop] | board.piece[
          white_queen_bishop]);
    }
  }

  if (const type_move_list* move_list = black_evasion(list, target); move_list - list > 1)
    depth--;
  type_move_list* p = list;

  while (p->move) {
    if ((p->move & 0x7fff) == trans_move)
      p->move |= 0xfff00000;
    p++;
  }
  p = list;
  int count = 0;

  while (p->move) {
    move = p->move;
    type_move_list* q = ++p;

    while (q->move) {
      if (move < q->move) {
        const uint32_t temp = q->move;
        q->move = move;
        move = temp;
      }
      q++;
    }

    if (move & 1 << 15 && score > -25000 && (move & 0x7fff) != trans_move && !black_see(move)) {
      count++;
      continue;
    }

    if (board.square[(move & 077)] == 0 && (move & 0x6000) == 0
      && (move & 0x7fff) != trans_move && position->flag & 1
      && static_cast<int>(max_increase[board.square[(move >> 6 & 077)]][move & 07777])
      + temp_position->score < score + 25 && score > -25000) {
      count++;
      continue;
    }
    move &= 0x7fff;
    black_make(move);
    eval(score - 150, score + 150, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check)
      value = -white_qsearch_check(1 - score, depth);
    else
      value = -white_qsearch(1 - score, depth);
    black_undo(move);

    if (value <= best_value)
      continue;
    best_value = value;

    if (value >= score) {
      hash_low(position->hash_key, move, 1, value);
      return value;
    }
  }

  if (count && best_value < -25000)
    best_value = score - 1;
  hash_high(position->hash_key, 1, best_value);
  return best_value;
}

int white_qsearch(const int score, const int depth) {
  int hash_score, i;
  const int k = position->hash_key & hash_mask;
  uint32_t move, trans_move = 0, move_depth = 0;
  type_move_list list[256];
  const type_position* temp_position = position;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->depth_low) {
        hash_score = hash->score_low;

        if (hash_score >= score)
          return hash_score;
      }

      if (hash->depth_high) {
        hash_score = hash->score_high;

        if (hash_score < score)
          return hash_score;
      }

      const uint32_t trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }
  int best_value = temp_position->score + 5;

  if (best_value >= score)
    return best_value;
  int value = score - 160;
  uint64_t target = board.piece[occupied_black];

  if (value > best_value) {
    value = score - 500;
    target ^= board.piece[black_pawn];

    if (value > best_value) {
      target ^= board.piece[black_knight] | (board.piece[black_queen_bishop] | board.piece[black_king_bishop]);
      value = score - 800;

      if (value > best_value)
        target ^= board.piece[black_rook];
    }

    if (board.piece[black_pawn] & position->white_attack)
      best_value += 160;
  }

  const type_move_list* move_list = white_capture(list, target);
  type_move_list* p = list;

  while (p->move) {
    if ((p->move & 0x7fff) == trans_move)
      p->move |= 0xffff0000;
    p++;
  }
  p = list;

  while (p->move) {
    move = p->move;
    type_move_list* q = ++p;

    while (q->move) {
      if (move < q->move) {
        const uint32_t temp = q->move;
        q->move = move;
        move = temp;
      }
      q++;
    }

    if (!(move & 0x300000) && (move & 0x7fff) != trans_move
      && square_set[(move >> 6 & 077)] & ~position->white_xray && !white_see(move))
      continue;
    move &= 0x7fff;
    white_make(move);
    eval(score - 150, score + 150, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check)
      value = -black_qsearch_check(1 - score, depth - 1);
    else
      value = -black_qsearch(1 - score, depth - 1);
    white_undo(move);

    if (value <= best_value)
      continue;
    best_value = value;

    if (value >= score) {
      hash_low(position->hash_key, move, 1, value);
      return value;
    }
  }

  if (depth >= -1 && temp_position->score >= score - (100 + (12 << (depth + 4)))) {
    move_list = white_check(list, target);

    for (i = 0; i < move_list - list; i++) {
      move = list[i].move;
      move &= 0x7fff;
      white_make(move);
      eval(score - 150, score + 150, move);

      if ((temp_position + 1)->white_king_check) {
        white_undo(move);
        continue;
      }
      value = -black_qsearch_check(1 - score, depth - 1);
      white_undo(move);

      if (value <= best_value)
        continue;
      best_value = value;

      if (value >= score) {
        hash_low(position->hash_key, move, 1, value);
        return value;
      }
    }
  }
  hash_high(position->hash_key, 1, best_value);
  return best_value;
}

int white_qsearch_check(const int score, int depth) {
  int hash_score, i, value,
      move_depth = 0;
  const int k = position->hash_key & hash_mask;
  type_move_list list[256];
  uint32_t move, trans_move = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;
  const type_position* temp_position = position;

  if (score < -30000 + 1)
    return -30000 + 1;

  if (score > 30000 - 1)
    return 30000 - 1;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->depth_low) {
        hash_score = hash->score_low;

        if (hash_score >= score)
          return hash_score;
      }

      if (hash->depth_high) {
        hash_score = hash->score_high;

        if (hash_score < score)
          return hash_score;
      }

      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }
  int best_value = temp_position - (root_position + 1) - 30000;
  uint64_t target = 0xffffffffffffffff;

  if (temp_position->score + 10 < score) {
    best_value = temp_position->score + 10;
    value = score - 200;
    target = board.piece[occupied_black];

    if (value > best_value) {
      target ^= board.piece[black_pawn];
      value = score - 500;
      best_value += 200;

      if (value > best_value)
        target ^= board.piece[black_knight] | (board.piece[black_queen_bishop] | board.piece[
          black_king_bishop]);
    }
  }

  if (const type_move_list* move_list = white_evasion(list, target); move_list - list > 1)
    depth--;
  type_move_list* p = list;

  while (p->move) {
    if ((p->move & 0x7fff) == trans_move)
      p->move |= 0xfff00000;
    p++;
  }
  p = list;
  int count = 0;

  while (p->move) {
    move = p->move;
    type_move_list* q = ++p;

    while (q->move) {
      if (move < q->move) {
        const uint32_t temp = q->move;
        q->move = move;
        move = temp;
      }
      q++;
    }

    if (move & 1 << 15 && score > -25000 && (move & 0x7fff) != trans_move && !white_see(move)) {
      count++;
      continue;
    }

    if (board.square[(move & 077)] == 0 && (move & 0x6000) == 0
      && (move & 0x7fff) != trans_move && position->flag & 2
      && static_cast<int>(max_increase[board.square[(move >> 6 & 077)]][move & 07777])
      + temp_position->score < score + 25 && score > -25000) {
      count++;
      continue;
    }
    move &= 0x7fff;
    white_make(move);
    eval(score - 150, score + 150, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check)
      value = -black_qsearch_check(1 - score, depth);
    else
      value = -black_qsearch(1 - score, depth);
    white_undo(move);

    if (value <= best_value)
      continue;
    best_value = value;

    if (value >= score) {
      hash_low(position->hash_key, move, 1, value);
      return value;
    }
  }

  if (count && best_value < -25000)
    best_value = score - 1;
  hash_high(position->hash_key, 1, best_value);
  return best_value;
}

int black_qsearch_pv(int alpha, const int beta, const int depth) {
  int i;
  uint32_t good_move = 0, trans_move = 0, move, bad_captures[64],
           move_depth = 0;
  int hash_score;
  type_move_list list[256];
  int value;
  const type_position* temp_position = position;
  const int k = position->hash_key & hash_mask;
  int bad_capture = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;

  if (beta < -30000)
    return -30000;

  if (alpha > 30000)
    return 30000;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->flag & 16) {
        hash_score = hash->score_high;
        return hash_score;
      }

      if (hash->depth_low) {
        hash_score = hash->score_low;

        if (hash_score >= beta)
          return hash_score;
      }

      if (hash->depth_high) {
        hash_score = hash->score_high;

        if (hash_score <= alpha)
          return hash_score;
      }

      const uint32_t trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }
  int best_value = temp_position->score + 5;
  uint64_t target = board.piece[occupied_white];

  if (best_value >= beta)
    return best_value;
  if (best_value > alpha)
    alpha = best_value;

  else {
    if (best_value < alpha - 160) {
      target ^= board.piece[white_pawn];

      if (best_value < alpha - 500) {
        target ^= board.piece[white_knight] | (board.piece[white_king_bishop] | board.piece[
          white_queen_bishop]);

        if (best_value < alpha - 800)
          target ^= board.piece[white_rook];
      }
      best_value += 160;
    }
  }

  const type_move_list* move_list = black_capture(list, target);
  type_move_list* p = list;

  while (p->move) {
    if ((p->move & 0x7fff) == trans_move)
      p->move |= 0xffff0000;
    p++;
  }
  p = list;

  while (p->move) {
    move = p->move;
    type_move_list* q = ++p;

    while (q->move) {
      if (move < q->move) {
        const int temp = q->move;
        q->move = move;
        move = temp;
      }
      q++;
    }

    if (move & 0x300000 || (move & 0x7fff) == trans_move || black_see(move)) {
      move &= 0x7fff;
      black_make(move);
      eval(-0x7fff0000, 0x7fff0000, move);

      if ((temp_position + 1)->black_king_check) {
        black_undo(move);
        continue;
      }

      if ((temp_position + 1)->white_king_check)
        value = -white_qsearch_pv_check(-beta, -alpha, depth - 1);
      else
        value = -white_qsearch_pv(-beta, -alpha, depth - 1);
      black_undo(move);

      if (value <= best_value)
        continue;
      best_value = value;

      if (value <= alpha)
        continue;
      alpha = value;
      good_move = move;

      if (value >= beta) {
        hash_low(position->hash_key, move, 1, value);
        return value;
      }
    }
    else
      bad_captures[bad_capture++] = move;
  }

  if (depth > 0)
    for (i = 0; i < bad_capture; i++) {
      move = bad_captures[i] & 0x7fff;
      black_make(move);
      eval(-0x7fff0000, 0x7fff0000, move);

      if ((temp_position + 1)->black_king_check) {
        black_undo(move);
        continue;
      }

      if ((temp_position + 1)->white_king_check)
        value = -white_qsearch_pv_check(-beta, -alpha, depth - 1);
      else
        value = -white_qsearch_pv(-beta, -alpha, depth - 1);
      black_undo(move);

      if (value <= best_value)
        continue;
      best_value = value;

      if (value <= alpha)
        continue;
      alpha = value;
      good_move = move;

      if (value >= beta) {
        hash_low(position->hash_key, move, 1, value);
        return value;
      }
    }

  if (depth >= -2 && temp_position->score >= alpha - (100 + (12 << (depth + 5)))) {
    move_list = black_check(list, target);

    for (i = 0; i < move_list - list; i++) {
      move = list[i].move & 0x7fff;
      black_make(move);
      eval(-0x7fff0000, 0x7fff0000, move);

      if ((temp_position + 1)->black_king_check) {
        black_undo(move);
        continue;
      }
      value = -white_qsearch_pv_check(-beta, -alpha, depth - 1);
      black_undo(move);

      if (value <= best_value)
        continue;
      best_value = value;

      if (value <= alpha)
        continue;
      alpha = value;
      good_move = move;

      if (value >= beta) {
        hash_low(position->hash_key, move, 1, value);
        return value;
      }
    }

    if (depth >= 0 && alpha <= temp_position->score + 150) {
      move_list = black_gain(list, alpha - temp_position->score);

      for (i = 0; i < move_list - list; i++) {
        move = list[i].move & 0x7fff;
        black_make(move);
        eval(-0x7fff0000, 0x7fff0000, move);

        if (-(temp_position + 1)->score < alpha) {
          black_undo(move);
          continue;
        }

        if ((temp_position + 1)->black_king_check || (temp_position + 1)->white_king_check) {
          black_undo(move);
          continue;
        }
        value = -white_qsearch_pv(-beta, -alpha, 0);
        black_undo(move);

        if (value <= best_value)
          continue;
        best_value = value;

        if (value <= alpha)
          continue;
        alpha = value;
        good_move = move;
        hash_low(position->hash_key, move, 1, value);

        if (value >= beta)
          return value;
      }
    }
  }

  if (good_move) {
    hash_exact(good_move, 1, best_value, 16);
    return best_value;
  }
  hash_high(position->hash_key, 1, best_value);
  return best_value;
}

int black_qsearch_pv_check(int alpha, const int beta, int depth) {
  int i;
  uint32_t trans_move = 0, good_move = 0, move;
  int hash_score;
  type_move_list list[256];
  const int k = position->hash_key & hash_mask;
  int value, move_depth = 0;
  const type_position* temp_position = position;

  if (beta < -30000)
    return -30000;

  if (alpha > 30000)
    return 30000;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->flag & 16) {
        hash_score = hash->score_high;
        return hash_score;
      }

      if (hash->depth_low) {
        hash_score = hash->score_low;

        if (hash_score >= beta)
          return hash_score;
      }

      if (hash->depth_high) {
        hash_score = hash->score_high;

        if (hash_score <= alpha)
          return hash_score;
      }

      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }
  int best_value = temp_position - (root_position + 1) - 30000;
  uint64_t target = 0xffffffffffffffff;

  if (temp_position->score + 10 < alpha) {
    best_value = temp_position->score + 10;
    value = alpha - 200;
    target = board.piece[occupied_white];

    if (value > best_value) {
      target ^= board.piece[white_pawn];
      value = alpha - 500;
      best_value += 200;

      if (value > best_value)
        target ^= board.piece[white_knight] | (board.piece[white_king_bishop] | board.piece[
          white_queen_bishop]);
    }
  }

  if (const type_move_list* move_list = black_evasion(list, target); move_list - list != 1)
    depth--;
  type_move_list* p = list;

  while (p->move) {
    if ((p->move & 0x7fff) == trans_move)
      p->move |= 0xfff00000;
    p++;
  }
  p = list;

  while (p->move) {
    move = p->move;
    type_move_list* q = ++p;

    while (q->move) {
      if (move < q->move) {
        const uint32_t temp = q->move;
        q->move = move;
        move = temp;
      }
      q++;
    }
    move &= 0x7fff;
    black_make(move);
    eval(-0x7fff0000, 0x7fff0000, move);

    if ((temp_position + 1)->black_king_check) {
      black_undo(move);
      continue;
    }

    if ((temp_position + 1)->white_king_check)
      value = -white_qsearch_pv_check(-beta, -alpha, depth);
    else
      value = -white_qsearch_pv(-beta, -alpha, depth);
    black_undo(move);

    if (value <= best_value)
      continue;
    best_value = value;

    if (value <= alpha)
      continue;
    alpha = value;
    good_move = move;
    hash_low(position->hash_key, move, 1, value);

    if (value >= beta)
      return value;
  }

  if (good_move) {
    hash_exact(good_move, 1, best_value, 16);
    return best_value;
  }
  hash_high(position->hash_key, 1, best_value);
  return best_value;
}

int white_qsearch_pv(int alpha, const int beta, const int depth) {
  int i;
  uint32_t good_move = 0, trans_move = 0, move, bad_captures[64],
           move_depth = 0;
  int hash_score;
  type_move_list list[256];
  int value;
  const type_position* temp_position = position;
  const int k = position->hash_key & hash_mask;
  int bad_capture = 0;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;

  if (beta < -30000)
    return -30000;

  if (alpha > 30000)
    return 30000;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->flag & 16) {
        hash_score = hash->score_high;
        return hash_score;
      }

      if (hash->depth_low) {
        hash_score = hash->score_low;

        if (hash_score >= beta)
          return hash_score;
      }

      if (hash->depth_high) {
        hash_score = hash->score_high;

        if (hash_score <= alpha)
          return hash_score;
      }

      const uint32_t trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }
  int best_value = temp_position->score + 5;
  uint64_t target = board.piece[occupied_black];

  if (best_value >= beta)
    return best_value;
  if (best_value > alpha)
    alpha = best_value;

  else {
    if (best_value < alpha - 160) {
      target ^= board.piece[black_pawn];

      if (best_value < alpha - 500) {
        target ^= board.piece[black_knight] | (board.piece[black_queen_bishop] | board.piece[
          black_king_bishop]);

        if (best_value < alpha - 800)
          target ^= board.piece[black_rook];
      }
      best_value += 160;
    }
  }

  const type_move_list* move_list = white_capture(list, target);
  type_move_list* p = list;

  while (p->move) {
    if ((p->move & 0x7fff) == trans_move)
      p->move |= 0xffff0000;
    p++;
  }
  p = list;

  while (p->move) {
    move = p->move;
    type_move_list* q = ++p;

    while (q->move) {
      if (move < q->move) {
        const int temp = q->move;
        q->move = move;
        move = temp;
      }
      q++;
    }

    if (move & 0x300000 || (move & 0x7fff) == trans_move || white_see(move)) {
      move &= 0x7fff;
      white_make(move);
      eval(-0x7fff0000, 0x7fff0000, move);

      if ((temp_position + 1)->white_king_check) {
        white_undo(move);
        continue;
      }

      if ((temp_position + 1)->black_king_check)
        value = -black_qsearch_pv_check(-beta, -alpha, depth - 1);
      else
        value = -black_qsearch_pv(-beta, -alpha, depth - 1);
      white_undo(move);

      if (value <= best_value)
        continue;
      best_value = value;

      if (value <= alpha)
        continue;
      alpha = value;
      good_move = move;

      if (value >= beta) {
        hash_low(position->hash_key, move, 1, value);
        return value;
      }
    }
    else
      bad_captures[bad_capture++] = move;
  }

  if (depth > 0)
    for (i = 0; i < bad_capture; i++) {
      move = bad_captures[i] & 0x7fff;
      white_make(move);
      eval(-0x7fff0000, 0x7fff0000, move);

      if ((temp_position + 1)->white_king_check) {
        white_undo(move);
        continue;
      }

      if ((temp_position + 1)->black_king_check)
        value = -black_qsearch_pv_check(-beta, -alpha, depth - 1);
      else
        value = -black_qsearch_pv(-beta, -alpha, depth - 1);
      white_undo(move);

      if (value <= best_value)
        continue;
      best_value = value;

      if (value <= alpha)
        continue;
      alpha = value;
      good_move = move;

      if (value >= beta) {
        hash_low(position->hash_key, move, 1, value);
        return value;
      }
    }

  if (depth >= -2 && temp_position->score >= alpha - (100 + (12 << (depth + 5)))) {
    move_list = white_check(list, target);

    for (i = 0; i < move_list - list; i++) {
      move = list[i].move & 0x7fff;
      white_make(move);
      eval(-0x7fff0000, 0x7fff0000, move);

      if ((temp_position + 1)->white_king_check) {
        white_undo(move);
        continue;
      }
      value = -black_qsearch_pv_check(-beta, -alpha, depth - 1);
      white_undo(move);

      if (value <= best_value)
        continue;
      best_value = value;

      if (value <= alpha)
        continue;
      alpha = value;
      good_move = move;

      if (value >= beta) {
        hash_low(position->hash_key, move, 1, value);
        return value;
      }
    }

    if (depth >= 0 && alpha <= temp_position->score + 150) {
      move_list = white_gain(list, alpha - temp_position->score);

      for (i = 0; i < move_list - list; i++) {
        move = list[i].move & 0x7fff;
        white_make(move);
        eval(-0x7fff0000, 0x7fff0000, move);

        if (-(temp_position + 1)->score < alpha) {
          white_undo(move);
          continue;
        }

        if ((temp_position + 1)->white_king_check || (temp_position + 1)->black_king_check) {
          white_undo(move);
          continue;
        }
        value = -black_qsearch_pv(-beta, -alpha, 0);
        white_undo(move);

        if (value <= best_value)
          continue;
        best_value = value;

        if (value <= alpha)
          continue;
        alpha = value;
        good_move = move;
        hash_low(position->hash_key, move, 1, value);

        if (value >= beta)
          return value;
      }
    }
  }

  if (good_move) {
    hash_exact(good_move, 1, best_value, 16);
    return best_value;
  }
  hash_high(position->hash_key, 1, best_value);
  return best_value;
}

int white_qsearch_pv_check(int alpha, const int beta, int depth) {
  int i;
  uint32_t trans_move = 0, good_move = 0, move;
  int hash_score;
  type_move_list list[256];
  const int k = position->hash_key & hash_mask;
  int value, move_depth = 0;
  const type_position* temp_position = position;

  if (beta < -30000)
    return -30000;

  if (alpha > 30000)
    return 30000;

  if (position->reversible >= 100)
    return 0;

  for (i = 4; i <= position->reversible && i <= stack_height; i += 2)
    if (stack[stack_height - i] == position->hash_key)
      return 0;

  for (i = 0; i < 4; i++) {
    if (const type_hash* hash = hash_table + (k + i); (hash->hash_key ^ position->hash_key >> 32) == 0) {
      if (hash->flag & 16) {
        hash_score = hash->score_high;
        return hash_score;
      }

      if (hash->depth_low) {
        hash_score = hash->score_low;

        if (hash_score >= beta)
          return hash_score;
      }

      if (hash->depth_high) {
        hash_score = hash->score_high;

        if (hash_score <= alpha)
          return hash_score;
      }

      const int trans_depth = hash->depth_low;
      move = hash->move;

      if (move && trans_depth > move_depth) {
        move_depth = trans_depth;
        trans_move = move;
      }
    }
  }
  int best_value = temp_position - (root_position + 1) - 30000;
  uint64_t target = 0xffffffffffffffff;

  if (temp_position->score + 10 < alpha) {
    best_value = temp_position->score + 10;
    value = alpha - 200;
    target = board.piece[occupied_black];

    if (value > best_value) {
      target ^= board.piece[black_pawn];
      value = alpha - 500;
      best_value += 200;

      if (value > best_value)
        target ^= board.piece[black_knight] | (board.piece[black_queen_bishop] | board.piece[
          black_king_bishop]);
    }
  }

  if (const type_move_list* move_list = white_evasion(list, target); move_list - list != 1)
    depth--;
  type_move_list* p = list;

  while (p->move) {
    if ((p->move & 0x7fff) == trans_move)
      p->move |= 0xfff00000;
    p++;
  }
  p = list;

  while (p->move) {
    move = p->move;
    type_move_list* q = ++p;

    while (q->move) {
      if (move < q->move) {
        const uint32_t temp = q->move;
        q->move = move;
        move = temp;
      }
      q++;
    }
    move &= 0x7fff;
    white_make(move);
    eval(-0x7fff0000, 0x7fff0000, move);

    if ((temp_position + 1)->white_king_check) {
      white_undo(move);
      continue;
    }

    if ((temp_position + 1)->black_king_check)
      value = -black_qsearch_pv_check(-beta, -alpha, depth);
    else
      value = -black_qsearch_pv(-beta, -alpha, depth);
    white_undo(move);

    if (value <= best_value)
      continue;
    best_value = value;

    if (value <= alpha)
      continue;
    alpha = value;
    good_move = move;
    hash_low(position->hash_key, move, 1, value);

    if (value >= beta)
      return value;
  }

  if (good_move) {
    hash_exact(good_move, 1, best_value, 16);
    return best_value;
  }
  hash_high(position->hash_key, 1, best_value);
  return best_value;
}
