// test/data/17_nested_ifdef.c
#define OUTER 1

int test() {
#ifdef OUTER
    int x = 1;
    #ifndef INNER
        int y = 2;
    #else
        int y = 3;
    #endif
#else
    #ifdef INNER
        int z = 4;
    #endif
    int x = 0;
#endif
    return x;
}
