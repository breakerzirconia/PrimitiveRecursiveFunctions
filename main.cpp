//
// Created by T3RCH1K and Nik Carlson
//

#include <iostream>
#include "Primitives.h"

using NatArgs = std::vector<unsigned>;

using Id = U<1, 1>;
using One = S<N, Z>;

using Sum = R<Id, S<N, U<3, 3>>>;
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

using Product = R<Z, S<Sum, U<3, 1>, U<3, 3>>>;
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

using LimitedDecrement = S<R<Z, U<3, 2>>, Id, Id>;
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

using Power = R<One, S<Product, U<3, 1>, U<3, 3>>>;
/*
 * Similar to Product
 */

using LimitedSub = R<Id, S<LimitedDecrement, U<3, 3>>>;
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

using FalseLess = S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedSub>;
/*
 * S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedSub>(1, 2)
 * R<One, S<Z, U<3, 1>>>(U<2, 1>(1, 2), LimitedSub(1, 2))
 * R<One, S<Z, U<3, 1>>>(1, 0)
 * S<One, Id>(1)
 * 1 - True
 *
 * S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedSub>(2, 2)
 * R<One, S<Z, U<3, 1>>>(2, 0)
 * One(2)
 * 1 - False
 *
 * S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedSub>(2, 1)
 * R<One, S<Z, U<3, 1>>>(2, 1)
 * S<Z, U<3, 1>>(2, 0, R<One, S<Z, U<3, 1>>>(2, 0))
 * S<Z, U<3, 1>>(2, 0, 1)
 * 0 - True
 */

using Less = S<R<One, S<Z, U<3, 1>>>, U<2, 1>, S<LimitedSub, S<N, U<2, 1>>, U<2, 2>>>;
// the correct implementation

using BitAnd = Z; // TODO, в рекурсии берём остаток от деления на два и делаем коньюнкцию
using BitXor = Z; // TODO, так же, как и выше
using First = Z; // TODO, ???
using Div = Z; // TODO, нужен FalseLess
using Mod = Z; // TODO, нужен FalseLess и LimitedSub
using Plog = Z; // TODO
using Factorial = R<One, S<Product, U<3, 1>, U<3, 2>>>; // TODO

template<typename P>
void printPRF([[maybe_unused]] P prf, const NatArgs& args) {
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
    printPRF(Less(), NatArgs{43, 12});
    printPRF(Less(), NatArgs{12, 12});
    printPRF(Less(), NatArgs{11, 12});
    return 0;
}