#ifndef UTILS_IOFORMAT_H
#define UTILS_IOFORMAT_H

#include <concepts>
#include <iostream>
#include <string>

// I LOVE CONCEPTS THANK YOU C++
template <typename Iterator, typename Lambda>
concept c_iterator_with_lambda_printer = requires(Iterator i, Iterator j, Lambda l) {
    *i;
    ++i;
    i != j;
    l(*i);
};

template <typename Iterator, typename Lambda> requires
c_iterator_with_lambda_printer<Iterator, Lambda>
void print_internally_separated_list(Iterator start, Iterator finish, std::string separator, Lambda &&l) {
    l(*start);
    ++start;
    for (; start != finish; ++start) {
        std::cout << separator;
        l(*start);
    }
}

#endif
