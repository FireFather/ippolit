#pragma once
#define PREFETCH(x) {}
#define FILE(s) ((s) & 7)
#define RANK(s) ((s) >> 3)
#define MAX(x, y) (((x) >= (y)) ? (x) : (y))
#define MIN(x, y) (((x) <= (y)) ? (x) : (y))
#define ABS(x) (((x) >= 0) ? (x) : -(x))
#define FILE_DISTANCE(x, y) (ABS(FILE(x) - FILE(y)))
#define RANK_DISTANCE(x, y) (ABS(RANK(x) - RANK(y)))
#define DISTANCE(i, j) (MAX (FILE_DISTANCE (i, j), RANK_DISTANCE (i, j)))
#define DISTANCE_KING_PAWN_WHITE(pawn, king) MAX ((((king) > (pawn)) ? 3 : 6) * ABS (RANK (pawn) - RANK (king)), 6 * ABS (FILE (pawn) - FILE (king)))
#define DISTANCE_KING_PAWN_BLACK(pawn, king) MAX ((((king) < (pawn)) ? 3 : 6) * ABS (RANK (pawn) - RANK (king)), 6 * ABS (FILE (pawn) - FILE (king)))
