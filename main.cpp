//
// Created by T3RCH1K and Nik Carlson
//

#include <iostream>
#include "Primitives.h"

using Id = U<1, 1>;
using One = S<N, Z>;

template<typename T>
using Flip2 = S<T, U<2, 2>, U<2, 1>>;

template<typename T>
using Duplicate = S<T, Id, Id>;

using Sum = R<Id, S<N, U<3, 3>>>;
/*
 * Sum(a, b) = a + b
 *
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
 * Product(a, b) = a * b
 *
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

using LimitedDecrement = Duplicate<R<Z, U<3, 2>>>;
/*
 * LimitedDecrement(a) = 0 if a < 0 else a - 1
 *
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
 * Power(a, b) = a ^ b
 *
 * Similar to Product
 */

using LimitedSub = R<Id, S<LimitedDecrement, U<3, 3>>>;
/*
 * LimitedSub(a, b) = 0 if a < b else a - b
 *
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

using LessOrEquals = S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedSub>;
/*
 * LessOrEquals(a, b) = 1 if a <= b else 0
 *
 * S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedSub>(1, 2)
 * R<One, S<Z, U<3, 1>>>(U<2, 1>(1, 2), LimitedSub(1, 2))
 * R<One, S<Z, U<3, 1>>>(1, 0)
 * S<One, Id>(1)
 * 1
 *
 * S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedSub>(2, 2)
 * R<One, S<Z, U<3, 1>>>(2, 0)
 * One(2)
 * 1
 *
 * S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedSub>(2, 1)
 * R<One, S<Z, U<3, 1>>>(2, 1)
 * S<Z, U<3, 1>>(2, 0, R<One, S<Z, U<3, 1>>>(2, 0))
 * S<Z, U<3, 1>>(2, 0, 1)
 * 0
 */

using Less = S<LessOrEquals, S<N, U<2, 1>>, U<2, 2>>;
/*
 * Less(a, b) = 1 if a < b else 0
 *
 * S<LessOrEquals, S<N, U<2, 1>>, U<2, 2>>(1, 2)
 * LessOrEquals(2, 2)
 * 1
 *
 * S<LessOrEquals, S<N, U<2, 1>>, U<2, 2>>(2, 2)
 * LessOrEquals(3, 2)
 * 0
 *
 * S<LessOrEquals, S<N, U<2, 1>>, U<2, 2>>(2, 1)
 * LessOrEquals(3, 1)
 * 0
 */

using Equals = S<Product, LessOrEquals, Flip2<LessOrEquals>>;
/*
 * Equals(a, b) = 1 if a == b else 0
 *
 * S<Product, LessOrEquals, S<LessOrEquals, U<2, 2>, U<2, 1>>>(4, 6)
 * Product(LessOrEquals(4, 6), S<LessOrEquals, U<2, 2>, U<2, 1>>(4, 6))
 * Product(LessOrEquals(4, 6), LessOrEquals(6, 4))
 * Product(1, 0)
 * 0
 *
 * S<Product, LessOrEquals, S<LessOrEquals, U<2, 2>, U<2, 1>>>(5, 5)
 * Product(LessOrEquals(5, 5), S<LessOrEquals, U<2, 2>, U<2, 1>>(5, 5))
 * Product(LessOrEquals(5, 5), LessOrEquals(5, 5))
 * Product(1, 1)
 * 1
 */

using Factorial = Duplicate<R<One, S<Product, S<N, U<3, 2>>, U<3, 3>>>>;
/*
 * Factorial(a) = a!
 *
 * S<R<One, S<Product, S<N, U<3, 2>>, U<3, 3>>>, Id, Id>(3)
 * R<One, S<Product, S<N, U<3, 2>>, U<3, 3>>>(3, 3)
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, R<One, S<Product, S<N, U<3, 2>>, U<3, 3>>>(3, 2))
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 1, R<One, S<Product, S<N, U<3, 2>>, U<3, 3>>>(3, 1)))
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 1, S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 0, R<One, S<Product, S<N, U<3, 2>>, U<3, 3>>>(3, 0))))
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 1, S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 0, One(3))))
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 1, S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 0, 1)))
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 1, Product(S<N, U<3, 2>>(3, 0, 1), U<3, 3>(3, 0, 1))))
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 1, Product(1, 1))
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 1, 1))
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, Product(S<N, U<3, 2>>(3, 1, 1), U<3, 3>(3, 1, 1)))
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, Product(2, 1))
 * S<Product, S<N, U<3, 2>>, U<3, 3>>(3, 2, 2)
 * Product(3, 2)
 * 6
 */

using If = R<U<2, 2>, U<4, 1>>;
/*
 * If(a, b, c) = b if c == 0 else a
 *
 * R<U<2, 2>, U<4, 1>>(3, 2, 0)
 * U<2, 2>(3, 2)
 * 2
 *
 * R<U<2, 2>, U<4, 1>>(3, 2, 1)
 * U<4, 1>(3, 2, 0, R<U<2, 2>, U<4, 1>>(3, 2, 0))
 * U<4, 1>(3, 2, 0, U<2, 2>(3, 2))
 * U<4, 1>(3, 2, 0, 2)
 * 3
 */

using Not = Duplicate<R<One, S<Z, U<3, 1>>>>;
/*
 * Not(a) = 1 if a == 0 else 0
 *
 * S<R<One, S<Z, U<3, 1>>>, Id, Id>(2)
 * R<One, S<Z, U<3, 1>>>(2, 2)
 * S<Z, U<3, 1>>(2, 1, R<One, S<Z, U<3, 1>>>(2, 1))
 * S<Z, U<3, 1>>(2, 1, S<Z, U<3, 1>>(2, 0, R<One, S<Z, U<3, 1>>>(2, 0)))
 * S<Z, U<3, 1>>(2, 1, S<Z, U<3, 1>>(2, 0, One(2)))
 * S<Z, U<3, 1>>(2, 1, S<Z, U<3, 1>>(2, 0, 2))
 * S<Z, U<3, 1>>(2, 1, 0)
 * 0
 *
 * S<R<One, S<Z, U<3, 1>>>, Id, Id>(0)
 * One(0)
 * 1
 */

using And = S<If, S<One, U<2, 1>>, S<Z, U<2, 1>>, Product>;
/*
 * And(a, b) = 0 if Product(a, b) == 0 else 1
 *
 * S<If, S<One, U<2, 1>>, S<Z, U<2, 1>>, Product>(2, 3)
 * If(1, 0, 6)
 * 1
 *
 * S<If, S<One, U<2, 1>>, S<Z, U<2, 1>>, Product>(5, 0)
 * If(1, 0, 0)
 * 0
 */

using Or = S<If, S<One, U<2, 1>>, S<Z, U<2, 1>>, Sum>;
/*
 * And(a, b) = 0 if Sum(a, b) == 0 else 1
 *
 * S<If, S<One, U<2, 1>>, S<Z, U<2, 1>>, Sum>(2, 3)
 * If(1, 0, 5)
 * 1
 *
 * S<If, S<One, U<2, 1>>, S<Z, U<2, 1>>, Sum>(0, 0)
 * If(1, 0, 0)
 * 0
 */

using NotEquals = S<Not, Equals>;
/*
 * NotEquals(a, b) = 0 if a == b else 1
 *
 * S<Not, Equals>(6, 4)
 * Not(Equals(6, 4))
 * Not(0)
 * 1
 *
 * S<Not, Equals>(5, 5)
 * Not(Equals(5, 5))
 * Not(1)
 * 0
 */

using Xor = NotEquals;
// synonym to NotEquals, since its semantics mimic the latter

using DivMax = Z; // TODO
using Div = S<R<Id, Id>, U<2, 1>, U<2, 2>, Z, U<2, 1>>; // TODO
using Mod = Z; // TODO
using BitAnd = Z; // TODO
using BitXor = Z; // TODO
using First = Z; // TODO
using Plog = Z; // TODO
using Pair = Z; // TODO
using IsPrime = Z; // TODO
using NthPrime = Z; // TODO

std::string listArgs(const NatArgs& args) {
    std::string result = "(";
    for (unsigned arg : args) {
        result.append(std::to_string(arg));
        result.append(", ");
    }
    if (!args.empty()) {
        result.std::string::erase(result.end() - 2, result.end());
    }
    result.append(")");
    return result;
}

template<typename P>
void printPRF([[maybe_unused]] P prf, const std::string& prfName, const NatArgs& args) {
    std::cout << prfName << listArgs(args) << " = " << P::compute(args) << "\n";
}

int main() {
    std::cout << "=== Sum ===" << "\n";
    printPRF(Sum(), "Sum", NatArgs{13, 12});
    printPRF(Sum(), "Sum", NatArgs{0, 2});
    printPRF(Sum(), "Sum", NatArgs{2, 0});
    printPRF(Sum(), "Sum", NatArgs{0, 0});
    std::cout << "=== Product ===" << "\n";
    printPRF(Product(), "Product", NatArgs{13, 12});
    printPRF(Product(), "Product", NatArgs{2, 0});
    printPRF(Product(), "Product", NatArgs{0, 2});
    printPRF(Product(), "Product", NatArgs{0, 0});
    std::cout << "=== Power ===" << "\n";
    printPRF(Power(), "Power", NatArgs{3, 6});
    printPRF(Power(), "Power", NatArgs{6, 3});
    std::cout << "=== LimitedDecrement ===" << "\n";
    printPRF(LimitedDecrement(), "LimitedDecrement", NatArgs{134});
    printPRF(LimitedDecrement(), "LimitedDecrement", NatArgs{12});
    printPRF(LimitedDecrement(), "LimitedDecrement", NatArgs{1});
    printPRF(LimitedDecrement(), "LimitedDecrement", NatArgs{0});
    std::cout << "=== LimitedSub ===" << "\n";
    printPRF(LimitedSub(), "LimitedSub", NatArgs{8, 6});
    printPRF(LimitedSub(), "LimitedSub", NatArgs{8, 8});
    printPRF(LimitedSub(), "LimitedSub", NatArgs{6, 8});
    std::cout << "=== LessOrEquals ===" << "\n";
    printPRF(LessOrEquals(), "LessOrEquals", NatArgs{43, 12});
    printPRF(LessOrEquals(), "LessOrEquals", NatArgs{12, 12});
    printPRF(LessOrEquals(), "LessOrEquals", NatArgs{11, 12});
    std::cout << "=== Less ===" << "\n";
    printPRF(Less(), "Less", NatArgs{43, 12});
    printPRF(Less(), "Less", NatArgs{12, 12});
    printPRF(Less(), "Less", NatArgs{11, 12});
    std::cout << "=== Equals ===" << "\n";
    printPRF(Equals(), "Equals", NatArgs{23, 41});
    printPRF(Equals(), "Equals", NatArgs{43, 21});
    printPRF(Equals(), "Equals", NatArgs{32, 32});
    std::cout << "=== Factorial ===" << "\n";
    printPRF(Factorial(), "Factorial", NatArgs{0});
    printPRF(Factorial(), "Factorial", NatArgs{1});
    printPRF(Factorial(), "Factorial", NatArgs{6});
    std::cout << "=== If ===" << "\n";
    printPRF(If(), "If", NatArgs{3, 4, 0});
    printPRF(If(), "If", NatArgs{3, 4, 1});
    printPRF(If(), "If", NatArgs{3, 4, 1111});
    std::cout << "=== Not ===" << "\n";
    printPRF(Not(), "Not", NatArgs{23});
    printPRF(Not(), "Not", NatArgs{1});
    printPRF(Not(), "Not", NatArgs{0});
    std::cout << "=== And ===" << "\n";
    printPRF(And(), "And", NatArgs{0, 0});
    printPRF(And(), "And", NatArgs{0, 1});
    printPRF(And(), "And", NatArgs{1, 0});
    printPRF(And(), "And", NatArgs{1, 1});
    std::cout << "=== Or ===" << "\n";
    printPRF(Or(), "Or", NatArgs{0, 0});
    printPRF(Or(), "Or", NatArgs{0, 1});
    printPRF(Or(), "Or", NatArgs{1, 0});
    printPRF(Or(), "Or", NatArgs{1, 1});
    std::cout << "=== NotEquals ===" << "\n";
    printPRF(NotEquals(), "NotEquals", NatArgs{23, 41});
    printPRF(NotEquals(), "NotEquals", NatArgs{21, 43});
    printPRF(NotEquals(), "NotEquals", NatArgs{32, 32});
    std::cout << "=== Xor ===" << "\n";
    printPRF(Xor(), "Xor", NatArgs{0, 0});
    printPRF(Xor(), "Xor", NatArgs{0, 1});
    printPRF(Xor(), "Xor", NatArgs{1, 0});
    printPRF(Xor(), "Xor", NatArgs{1, 1});

    return 0;
}