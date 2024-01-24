#ifndef UTILS_IOFORMAT_H
#define UTILS_IOFORMAT_H

#include <concepts>
#include <iostream>
#include <string>

class ioformat {
public:
    static constexpr char const * const GRAY   = "\e[0;90m";
    static constexpr char const * const RED    = "\e[0;91m";
    static constexpr char const * const GREEN  = "\e[0;92m";
    static constexpr char const * const YELLOW = "\e[0;93m";
    static constexpr char const * const BLUE   = "\e[0;94m";
    static constexpr char const * const PURPLE = "\e[0;95m";
    static constexpr char const * const CYAN   = "\e[0;96m";
    static constexpr char const * const WHITE  = "\e[0;97m";
    static constexpr char const * const RESET  = "\e[0m";
};

// I LOVE CONCEPTS THANK YOU C++
template <typename Iterator, typename Callable>
concept c_iterator_with_lambda_printer = requires(Iterator i, Iterator j, Callable &&l) {
    *i;
    ++i;
    i != j;
    l(*i);
};

template <typename Iterator, typename Callable> requires
c_iterator_with_lambda_printer<Iterator, Callable>
void print_internally_separated_list(std::ostream &os, Iterator start, Iterator finish, std::string separator, Callable &&l) {
    if (!(start != finish)) {
        return;
    }
    l(*start);
    ++start;
    for (; start != finish; ++start) {
        os << separator;
        l(*start);
    }
}

#endif
