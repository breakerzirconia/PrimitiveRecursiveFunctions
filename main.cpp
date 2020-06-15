//
// Created by T3RCH1K and Nik Carlson
//

#include <iostream>
#include "Primitives.h"

using NatArgs = std::vector<unsigned>;

typedef U<1, 1> Id;
typedef S<N, Z> One;
typedef S<N, One> Two;

typedef R<Id, S<N, U<3, 3>>> Sum;
/*
 * R<U<1, 1>, S<N, U<3, 3>>>(4, 2)
 * S<N, U<3, 3>>(4, 1, R<U<1, 1>, S<N, U<3, 3>>>(4, 1))
 * S<N, U<3, 3>>(4, 1, S<N, U<3, 3>>(4, 0, R<U<1, 1>, S<N, U<3, 3>>>(4, 0)))
 * S<N, U<3, 3>>(4, 1, S<N, U<3, 3>>(4, 0, U<1, 1>(4)))
 * S<N, U<3, 3>>(4, 1, S<N, U<3, 3>>(4, 0, 4))
 * S<N, U<3, 3>>(4, 1, N(U<3, 3>(4, 0, 4)))
 * S<N, U<3, 3>>(4, 1, N(4))
 * S<N, U<3, 3>>(4, 1, 5)
 * N(U<3, 3>(4, 1, 5))
 * N(5)
 * 6
 */

typedef R<Z, S<Sum, U<3, 1>, U<3, 3>>> Product;
/*
 * R<Z, S<Sum, U<3, 1>, U<3, 3>>>(4, 2)
 * S<Sum, U<3, 1>, U<3, 3>>(4, 1, R<Z, S<Sum, U<3, 1>, U<3, 3>>>(4, 1))
 * S<Sum, U<3, 1>, U<3, 3>>(4, 1, S<Sum, U<3, 1>, U<3, 3>>(4, 0, R<Z, S<Sum, U<3, 1>, U<3, 3>>>(4, 0)))
 * S<Sum, U<3, 1>, U<3, 3>>(4, 1, S<Sum, U<3, 1>, U<3, 3>>(4, 0, Z(4)))
 * S<Sum, U<3, 1>, U<3, 3>>(4, 1, S<Sum, U<3, 1>, U<3, 3>>(4, 0, 0))
 * S<Sum, U<3, 1>, U<3, 3>>(4, 1, Sum(U<3, 1>(4, 0, 0), U<3, 3>(4, 0, 0)))
 * S<Sum, U<3, 1>, U<3, 3>>(4, 1, Sum(4, 0))
 * Sum(U<3, 1>(4, 1, 4), U<3, 3>(4, 1, 4))
 * Sum(4, 4)
 * 8
 */

typedef R<Z, U<3, 2>> LimitedMinusOne;//работает только от двух аргументов
typedef Z LimitedMinus;//не сделано
typedef Z Less;//не сделано
typedef Z BitAnd;// не сделано, в рекурсии берём остаток от деления на два и делаем коньюнкцию
typedef Z BitXor;//не сделано, так же, как и выше
typedef Z First;//не сделано, ???
typedef Z Div;//не сделано, нужен Less
typedef Z Mod;//не сделано, нужен Less и LimitedMinus
typedef R<One, S<Product, U<3, 1>, U<3, 3>>> Power;
/*
 * a ^^ b = { a ^ a ^ a ^ ... ^ a } b times
 */

typedef Z Plog;// не сделано
typedef R<One, S<Product, U<3, 1>, U<3, 2>>> Factorial;//не сделано

template<typename P>
void printPRF(P prf, const NatArgs& args) {
    std::cout << P::compute(args) << "\n";
}

int main() {
    printPRF(Sum(), NatArgs{13, 12});
    printPRF(Product(), NatArgs{13, 12});
    printPRF(Power(), NatArgs{3, 6});
    return 0;
}