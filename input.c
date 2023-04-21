i32 helper(int i, int j) {
    return i * j;
}

i64 foo() {
    i32 l = 0;
    i32 m = helper;
    return 364;
}

i32 main() {
    i64 myvar1 = (foo(arg1, arg2, arg3));
    i32 myvar2 = (1 - *myvar1++)() * 3 / (4, (myvar1, 6 - 7, 8));
    myvar1 = myvar2 * 3;
    return myvar1;
}