cargo run > out.c &&
gcc -S -static -O3 -Wall -Wextra out.c 