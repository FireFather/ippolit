#pragma once

#define ENGINE "Ippolit"
#define VERSION "051323"
#define AUTHOR "Yakov Petrovich Golyadkin"

#ifdef _WIN64
#define PLATFORM "x64"
#else
#define PLATFORM "w32"
#endif

#define mat_pawn_value 32651535390539850
#define mat_knight_value 95984289925890303
#define mat_bishop_value 99924995435856148
#define mat_rook_value 168605830360924557
#define mat_queen_value 330174640275850032
#define mat_bishop_pair_value 12948029319479325

#define mat_knight_pawn_bonus 1407392063553536
#define mat_rook_pawn_penalty 8590196741
#define mat_queen_rook_penalty 4503659757568008
#define mat_2_rook_penalty 9007319515136016
