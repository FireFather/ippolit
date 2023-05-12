#pragma once
#define MEMALIGN(a,b,c) a=malloc(c)
#define prefetch(x) {}
#define FILE(s) ((s) & 7)
#define RANK(s) ((s) >> 3)
#define MAX(x, y) (((x) >= (y)) ? (x) : (y))
#define MIN(x, y) (((x) <= (y)) ? (x) : (y))
#define ABS(x) (((x) >= 0) ? (x) : -(x))
#define file_distance(x, y) (ABS(FILE(x) - FILE(y)))
#define rank_distance(x, y) (ABS(RANK(x) - RANK(y)))
#define distance(i, j) (MAX (file_distance (i, j), rank_distance (i, j)))
#define distance_king_pawn_white(pawn, king) MAX ((((king) > (pawn)) ? 3 : 6) * ABS (RANK (pawn) - RANK (king)), 6 * ABS (FILE (pawn) - FILE (king)))
#define distance_king_pawn_black(pawn, king) MAX ((((king) < (pawn)) ? 3 : 6) * ABS (RANK (pawn) - RANK (king)), 6 * ABS (FILE (pawn) - FILE (king)))
