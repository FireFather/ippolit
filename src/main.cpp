
#include "pragma.h"
#include "functions.h"

int main() {
  engine_info();
  init_hash();
  init_pawn_hash_key();
  clear_history();
  init_captures();
  init_arrays();
  init_material();
  init_game();

  while (true)
    read_input();
}
