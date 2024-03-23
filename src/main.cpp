#include <windows.h>
#include <cstdio>
#include <intrin.h>

#include "pragma.h"
#include "defines.h"
#include "functions.h"

int main() {
  const auto startup_banner = "" ENGINE " " VERSION " " PLATFORM "\n\n";
  printf(startup_banner);
  fflush(stdout);
  init_pawn_hash_key();
  init_hash();
  clear_history();
  init_captures();
  init_arrays();
  init_material();
  init_game();

  while (true) input();
}
