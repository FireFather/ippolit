
static uint16 rand16( void )
    {
	randkey = randkey * 8765432181103515245 + 1234567891;
    return (( randkey >> 32) % 65536);
    }

static uint64 rand64( void )
    {
    return (((uint64)rand16()) << 48) | (((uint64)rand16()) << 32) | (((uint64)rand16()) << 16)
	| (((uint64)rand16()) << 0);
    }
