#include <stdio.h>

extern char my_main(int, char**);

int main(int argc, char **argv) {
    char res = my_main(argc, argv);
    printf("%c\n", res);
    return 0;
}
