#pragma once
static uint16_t rand16()
{
	randkey = randkey * 8765432181103515245 + 1234567891;
	return (randkey >> 32) % 65536;
}

inline uint64_t rand64()
{
	return static_cast<uint64_t>(rand16()) << 48 | static_cast<uint64_t>(rand16()) << 32 | static_cast<uint64_t>(rand16()) << 16
		| static_cast<uint64_t>(rand16()) << 0;
}
