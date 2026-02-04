#include <stdio.h>

extern int my_main(int, char**);

int main() {
    int res = my_main(0, NULL);
    printf("%d\n", res);
    return 0;
}
