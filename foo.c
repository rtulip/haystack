#include <stdio.h>
#include <stdint.h>

int foo() {
    printf("inside foo!\n");
    return 4;
}

void bar(uint32_t x)
{
    printf("Bar: %u\n", x);
}