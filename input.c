decl (i32, *i32) *() i32
=> foo () {
    return i * j;
}

decl () i64
=> bar () {
    decl i32 l = 0;
    decl i32 m = helper;
    return 40;
}

decl () i32
=> main () {
    using hehe = []*i32;
    //typedef []*i32 hehe;
    decl i64 myvar1 = (foo());
    decl []*i32 myvar2 = (1 - myvar1++)() * 3 / (4, (myvar1, 6 - 7, 8));
    decl hehe myvar3 = myvar2;
    //typedef hehe mimi;
    decl mimi myvar4 = myvar2 = 4;
    return myvar1;
}
