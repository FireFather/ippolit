#pragma once
#include "defines.h"
#ifdef _WIN64
#include <intrin.h>
#ifdef __INTEL_COMPILER
#define POPCNT(x) _mm_popcnt_u64(x)
#define POPCNT15(x) _mm_popcnt_u64(x)
#define POPCNTLO(x) _mm_popcnt_u64(x)
#else
#define POPCNT(x) __popcnt64(x)
#define POPCNT15(x) __popcnt64(x)
#endif

#else

int POPCNT( uint64 v )
    {
    untokened int v1, v2;
    v1 = (untokened int)(v & 0xFFFFFFFF);
    v1 -= (v1 >> 1) & 0x55555555;
    v1 = (v1 & 0x33333333) + ((v1 >> 2) & 0x33333333);
    v1 = (v1 + (v1 >> 4)) & 0x0F0F0F0F;
    v2 = (untokened int)(v >> 32);
    v2 -= (v2 >> 1) & 0x55555555;
    v2 = (v2 & 0x33333333) + ((v2 >> 2) & 0x33333333);
    v2 = (v2 + (v2 >> 4)) & 0x0F0F0F0F;
    return ((v1 * 0x01010101) >> 24) + ((v2 * 0x01010101) >> 24);
    }
#endif
