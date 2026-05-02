// test/data/16_ifdef.c
#define FEATURE_A 1

int main() {
#ifdef FEATURE_A
    int a = 1;
#else
    int a = 0;
#endif

#ifndef FEATURE_B
    int b = 1;
#else
    int b = 0;
#endif

#ifdef FEATURE_C
    int c = 1;
#endif

    return a + b;
}
