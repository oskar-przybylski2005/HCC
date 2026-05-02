// test/data/15_include_macro.c
#include "15_include_macro.h"

int main() {
    float r = PI;
    if (DEBUG_MODE) {
        return 0;
    }
    return 1;
}
