// test/data/18_if_expr.c
#define X 10
#define Y 20

int main() {
#if X > 5 && Y < 30
    int a = 1;
#endif

#if defined(X) || defined Z
    int b = 1;
#endif

#if X * 2 == Y
    int c = 1;
#endif

#if !defined(UNKNOWN)
    int d = 1;
#endif

#if UNKNOWN_VAR > 0
    int bad = 1;
#else
    int good = 1;
#endif

#if 0
    int false1 = 1;
#elif X == 10
    int good2 = 1;
#else
    int false2 = 1;
#endif

    return 0;
}
