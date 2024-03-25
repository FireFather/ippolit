cls
arm-linux-gnueabi-g++.exe -o ippolit main.cpp ippolit.cpp -march=armv5te -mtune=arm7 -Ofast -finline-functions -flto -static -static-libgcc -static-libstdc++ -lm -Wl,--whole-archive -Wl,--no-whole-archive -lstdc++ -std=c++14 -Wno-attributes -Wno-aggressive-loop-optimizations
          