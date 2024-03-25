cls
g++ -o ippolit main.cpp ippolit.cpp -flto -Ofast -march=native -mtune=native -static -static -static-libgcc -static-libstdc++ -mpopcnt -finline-functions -Wno-aggressive-loop-optimizations
          