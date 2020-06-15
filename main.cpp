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

typedef S<R<Z, U<3, 2>>, Id, Id> LimitedDecrement;
/*
 * S<R<Z, U<3, 2>>, Id, Id>(3)
 * R<Z, U<3, 2>>(Id(3), Id(3))
 * R<Z, U<3, 2>>(3, 3)
 * U<3, 2>(3, 2, R<Z, U<3, 2>>(3, 2))
 * U<3, 2>(3, 2, U<3, 2>(3, 1, R<Z, U<3, 2>>(3, 1)))
 * U<3, 2>(3, 2, U<3, 2>(3, 1, U<3, 2>(3, 0, R<Z, U<3, 2>>(3, 0))))
 * U<3, 2>(3, 2, U<3, 2>(3, 1, U<3, 2>(3, 0, 0)))
 * U<3, 2>(3, 2, U<3, 2>(3, 1, 0))
 * U<3, 2>(3, 2, 1)
 * 2
 */

typedef R<One, S<Product, U<3, 1>, U<3, 3>>> Power;
/*
 * Similar to Product
 */

typedef R<Id, S<LimitedDecrement, U<3, 3>>> LimitedSub; // TODO
/*
 * R<Id, LD3AR3rd>(8, 3)
 * LD3AR3rd(8, 2, R<Id, LD3AR3rd>(8, 2))
 * LD3AR3rd(8, 2, LD3AR3rd(8, 1, R<Id, LD3AR3rd>(8, 1)))
 * LD3AR3rd(8, 2, LD3AR3rd(8, 1, LD3AR3rd(8, 0, R<Id, LD3AR3rd>(8, 0))))
 * LD3AR3rd(8, 2, LD3AR3rd(8, 1, LD3AR3rd(8, 0, Id(8))))
 * LD3AR3rd(8, 2, LD3AR3rd(8, 1, LD3AR3rd(8, 0, 8)))
 * LD3AR3rd(8, 2, LD3AR3rd(8, 1, 7))
 * LD3AR3rd(8, 2, 6)
 * 5
 *
 * the case when a < b is analogous
 */

typedef Z Less; // TODO
/*
 *
 */

typedef Z BitAnd; // TODO, в рекурсии берём остаток от деления на два и делаем коньюнкцию
typedef Z BitXor; // TODO, так же, как и выше
typedef Z First; // TODO, ???
typedef Z Div; // TODO, нужен Less
typedef Z Mod; // TODO, нужен Less и LimitedSub
typedef Z Plog; // TODO
typedef R<One, S<Product, U<3, 1>, U<3, 2>>> Factorial; // TODO

template<typename P>
void printPRF(P prf, const NatArgs& args) {
    std::cout << P::compute(args) << "\n";
}

int main() {
    printPRF(Sum(), NatArgs{13, 12});
    printPRF(Product(), NatArgs{13, 12});
    printPRF(Power(), NatArgs{3, 6});
    printPRF(LimitedDecrement(), NatArgs{134});
    printPRF(LimitedDecrement(), NatArgs{12});
    printPRF(LimitedDecrement(), NatArgs{1});
    printPRF(LimitedDecrement(), NatArgs{0});
    printPRF(LimitedSub(), NatArgs{8, 6});
    printPRF(LimitedSub(), NatArgs{8, 8});
    printPRF(LimitedSub(), NatArgs{6, 8});
    return 0;
}