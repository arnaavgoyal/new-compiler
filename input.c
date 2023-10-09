{
    var i32 i;
}

// while (i < 10) {
//     // e
// };

using MYTYPE = u32;

def (MYTYPE, i32) ()i32
foo (first, second) {
    var i32 i;
    var i64 j;
    var MYTYPE first; // should give redecl error
    var i64 no = (0);
    foo(first as i32, second);
    def (MYTYPE)MYTYPE food (first) {
        var i32 bar;
        foo(first, bar);
    };
    return i * j;
};

def () *MYTYPE
bar () {
    return 1;
};

// var (i64, f64) i64
// bar (x,,z) {
//     var i32 l = 0;
//     var i32 m = helper;
//     return 40;
// };

def () i32
main () {
    using MYTYPE2 = ()**MYTYPE;
    var MYTYPE2 myvar1;
    var i32 myvar2 = 0;
    //var *() i32 myvar1 = (foo());
    //var MYTYPE2 myvar2 = (1 - *&myvar1[7]++)() * 3 / (4, (myvar1, 6 - 7, 8));
    //func hehe myvar3 = myvar2;
    //mimi = myvar4 = ;
    return myvar1();
};