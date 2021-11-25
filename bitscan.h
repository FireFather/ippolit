#ifdef x64

__inline int BSF( uint64 x )
    {
    int y;
    _BitScanForward64(&y, x);
    return y;
    }

__inline int BSR( uint64 x )
    {
    int y;
    _BitScanReverse64(&y, x);
    return y;
    }

#else

int BSF( uint64 x )
    {
    	{
        _asm
            {
            mov eax, dword ptr x[0]
			test eax, eax
			jz f_hi
			bsf eax, eax
			jmp f_ret
			f_hi: bsf eax, dword ptr x[4]
			add eax, 20h
			f_ret:
            }
    	}
    }

int BSR( uint64 x )
    {
   		{
        _asm
            {
            mov eax, dword ptr x[4]
			test eax, eax
			jz l_lo
			bsr eax, eax
			add eax, 20h
			jmp l_ret
			l_lo: bsr eax, dword ptr x[0]
			l_ret:
            }
    	}
    }

#endif