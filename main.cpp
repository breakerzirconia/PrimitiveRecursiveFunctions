//
// Created by T3RCH1K and Nik Carlson
//

#include <iostream>
#include <string>
#include "Primitives.h"

using Id = U<1, 1>;
using One = S<N, Z>;
using Two = S<N, One>;

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

using LimitedDifference = R<Id, S<LimitedDecrement, U<3, 3>>>;
/*
 * LimitedDifference(a, b) = 0 if a < b else a - b
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

using LessOrEquals = S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedDifference>;
/*
 * LessOrEquals(a, b) = 1 if a <= b else 0
 *
 * S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedDifference>(1, 2)
 * R<One, S<Z, U<3, 1>>>(U<2, 1>(1, 2), LimitedDifference(1, 2))
 * R<One, S<Z, U<3, 1>>>(1, 0)
 * S<One, Id>(1)
 * 1
 *
 * S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedDifference>(2, 2)
 * R<One, S<Z, U<3, 1>>>(2, 0)
 * One(2)
 * 1
 *
 * S<R<One, S<Z, U<3, 1>>>, U<2, 1>, LimitedDifference>(2, 1)
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

using Ternary = S<If, U<3, 2>, U<3, 3>, U<3, 1>>;
// Ternary(c, a, b) = If(a, b, c)

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

using DivDetermine = S<Ternary, S<Less, S<LimitedDifference, U<4, 1>, S<Product, U<4, 4>, U<4, 2>>>, U<4, 2>>, U<4, 4>, S<N, U<4, 4>>>;
/*
 * S<Ternary, S<Less, S<LimitedDifference, U<5, 1>, S<Product, U<5, 4>, U<5, 2>>>, U<5, 2>>, U<5, 4>, S<N, U<4, 4>>>(5, 2, 0, 0)
 * Ternary(S<Less, S<LimitedDifference, U<5, 1>, S<Product, U<5, 4>, U<5, 2>>>, U<5, 2>>(5, 2, 0, 0, 1), 0, 1)
 * Ternary(Less(S<LimitedDifference, U<5, 1>, S<Product, U<5, 4>, U<5, 2>>>(5, 2, 0, 0, 1), 2), 0, 1)
 * Ternary(Less(LimitedDifference(5, S<Product, U<5, 4>, U<5, 2>>(5, 2, 0, 0, 1)), 2), 0, 1)
 * Ternary(Less(LimitedDifference(5, 0), 2), 0, 1)
 * Ternary(Less(5, 2), 0, 1)
 * Ternary(0, 0, 1)
 * 1
 *
 * S<Ternary, S<Less, S<LimitedDifference, U<5, 1>, S<Product, U<5, 4>, U<5, 2>>>, U<5, 2>>, U<5, 4>, S<Sum, U<5, 4>, U<5, 5>>>(5, 2, 1, 1)
 * Ternary(S<Less, S<LimitedDifference, U<5, 1>, S<Product, U<5, 4>, U<5, 2>>>, U<5, 2>>(5, 2, 1, 1, 1), 1, 2)
 * Ternary(Less(S<LimitedDifference, U<5, 1>, S<Product, U<5, 4>, U<5, 2>>>(5, 2, 1, 1, 1), 2), 1, 2)
 * Ternary(Less(LimitedDifference(5, S<Product, U<5, 4>, U<5, 2>>(5, 2, 1, 1, 1)), 2), 1, 2)
 * Ternary(Less(LimitedDifference(5, 2), 2), 1, 2)
 * Ternary(Less(3, 2), 1, 2)
 * Ternary(0, 1, 2)
 * 2
 *
 * S<Ternary, S<Less, S<LimitedDifference, U<5, 1>, S<Product, U<5, 4>, U<5, 2>>>, U<5, 2>>, U<5, 4>, S<Sum, U<5, 4>, U<5, 5>>>(5, 2, 2, 2)
 * Ternary(S<Less, S<LimitedDifference, U<5, 1>, S<Product, U<5, 4>, U<5, 2>>>, U<5, 2>>(5, 2, 2, 2, 1), 2, 3)
 * Ternary(Less(S<LimitedDifference, U<5, 1>, S<Product, U<5, 4>, U<5, 2>>>(5, 2, 2, 2, 1), 2), 2, 3)
 * Ternary(Less(LimitedDifference(5, S<Product, U<5, 4>, U<5, 2>>(5, 2, 2, 2, 1)), 2), 2, 3)
 * Ternary(Less(LimitedDifference(5, 4), 2), 2, 3)
 * Ternary(Less(1, 2), 2, 3)
 * Ternary(1, 2, 3)
 * 2
 *
 * etc... same for 3, 4, 5
 */

using Div = S<R<S<Z, U<2, 1>>, DivDetermine>, U<2, 1>, U<2, 2>, U<2, 1>>;
/*
 * Div(a, b) = floor(a / b)
 *
 * S<R<S<Z, U<2, 1>>, DivDetermine>, U<2, 1>, U<2, 2>, U<2, 1>>(5, 2)
 * R<S<Z, U<2, 1>>, DivDetermine>(5, 2, 0) = S<Z, U<2, 1>>(5, 2) = 0
 * R<S<Z, U<2, 1>>, DivDetermine>(5, 2, 1) =
 *     DivDetermine(5, 2, 0, 0, 1) = 1
 * R<S<Z, U<2, 1>>, DivDetermine>(5, 2, 2) =
 *     DivDetermine(5, 2, 1, 1, 1) = 2
 * R<S<Z, U<2, 1>>, DivDetermine>(5, 2, 3) =
 *     DivDetermine(5, 2, 2, 2, 1) = 2
 * R<S<Z, U<2, 1>>, DivDetermine>(5, 2, 4) =
 *     DivDetermine(5, 2, 3, 2, 1) = 2
 * R<S<Z, U<2, 1>>, DivDetermine>(5, 2, 5) =
 *     DivDetermine(5, 2, 3, 1, 1) = 2
 */

using Mod = S<LimitedDifference, U<2, 1>, S<Product, U<2, 2>, Div>>;
/*
 * Mod(a, b) = a % b
 *
 * S<LimitedDifference, U<2, 1>, S<Product, U<2, 2>, Div>>(5, 2)
 * LimitedDifference(5, Product(2, 2))
 * 1
 * S<LimitedDifference, U<2, 1>, S<Product, U<2, 2>, Div>>(2, 5)
 * LimitedDifference(2, Product(2, 0))
 * 2
 */

using IsPrime = Duplicate<R<Z, S<Ternary, S<Equals, U<3, 2>, S<One, U<3, 1>>>, S<One, U<3, 1>>, S<Ternary, S<Mod, U<3, 1>, U<3, 2>>, S<Product, S<One, U<3, 1>>, U<3, 3>>, S<Z, U<3, 1>>>>>>;
/*
 * IsPrime(a) = 1 if a is prime else 0
 *
 * R<>(3, 0) = 0
 * R<>(3, 1) = S<Ternary, S<Equals, U<3, 2>, S<One, U<3, 1>>>, S<One, U<3, 1>>, S<Ternary, S<Mod, U<3, 1>, U<3, 2>>, S<Product, S<One, U<3, 1>>, U<3, 3>>, S<Z, U<3, 1>>>>(3, 0, 0)
 *           = Ternary(Equals(0, 1), 1, Ternary(Mod(3, 0), Product(1, 0), 0))
 *           = Ternary(0, 1, Ternary(3, 0, 0))
 *           = 0
 * R<>(3, 2) = S<Ternary, S<Equals, U<3, 2>, S<One, U<3, 1>>>, S<One, U<3, 1>>, S<Ternary, S<Mod, U<3, 1>, U<3, 2>>, S<Product, S<One, U<3, 1>>, U<3, 3>>, S<Z, U<3, 1>>>>(3, 1, 0)
 *           = Ternary(Equals(1, 1), 1, Ternary(Mod(3, 1), Product(1, 0), 0))
 *           = Ternary(1, 1, Ternary(0, 0, 0))
 *           = 1
 * R<>(3, 3) = S<Ternary, S<Equals, U<3, 2>, S<One, U<3, 1>>>, S<One, U<3, 1>>, S<Ternary, S<Mod, U<3, 1>, U<3, 2>>, S<Product, S<One, U<3, 1>>, U<3, 3>>, S<Z, U<3, 1>>>>(3, 2, 1)
 *           = Ternary(Equals(2, 1), 1, Ternary(Mod(3, 2), Product(1, 1), 0))
 *           = Ternary(0, 1, Ternary(1, 1, 0))
 *           = 1
 *
 *
 */

using PlogR = R<S<Z, U<2, 1>>, S<Ternary, S<Equals, S<Mod, U<4, 1>, S<Power, U<4, 2>, U<4, 4>>>, S<Z, U<4, 1>>>, S<N, U<4, 4>>, U<4, 4>>>;

using Plog = S<LimitedDecrement, S<PlogR, U<2, 2>, U<2, 1>, U<2, 2>>>;
/*
 * Plog(base, a) = {max p | a is divisible by base ^ p}
 *
 * S<LimitedDecrement, S<PlogR, U<2, 1>, U<2, 2>, U<2, 1>>>(72, 6)
 * LimitedDecrement(R<>(72, 6, 72))
 * R<>(72, 6, 0) = 0
 * R<>(72, 6, 1) = S<Ternary, S<Equals, S<Mod, U<4, 1>, S<Power, U<4, 2>, U<4, 4>>>, S<Z, U<4, 1>>>, S<N, U<4, 4>>, U<4, 4>>(72, 6, 0, 0)
 *               = Ternary(Equals(Mod(72, Power(6, 0)), 0), 1, 0)
 *               = Ternary(1, 1, 0)
 *               = 1
 * R<>(72, 6, 2) = S<Ternary, S<Equals, S<Mod, U<4, 1>, S<Power, U<4, 2>, U<4, 4>>>, S<Z, U<4, 1>>>, S<N, U<4, 4>>, U<4, 4>>(72, 6, 1, 1)
 *               = Ternary(Equals(Mod(72, Power(6, 1)), 0), 2, 1)
 *               = Ternary(1, 2, 1)
 *               = 2
 * R<>(72, 6, 3) = S<Ternary, S<Equals, S<Mod, U<4, 1>, S<Power, U<4, 2>, U<4, 4>>>, S<Z, U<4, 1>>>, S<N, U<4, 4>>, U<4, 4>>(72, 6, 2, 2)
 *               = Ternary(Equals(Mod(72, Power(6, 2)), 0), 3, 2)
 *               = Ternary(1, 3, 2)
 *               = 3
 * R<>(72, 6, 4) = S<Ternary, S<Equals, S<Mod, U<4, 1>, S<Power, U<4, 2>, U<4, 4>>>, S<Z, U<4, 1>>>, S<N, U<4, 4>>, U<4, 4>>(72, 6, 3, 3)
 *               = Ternary(Equals(Mod(72, Power(6, 3)), 0), 4, 3)
 *               = Ternary(0, 4, 3)
 *               = 3
 * etc... same for 5, 6, ..., 72
 */

using IntermediateSqrt = Duplicate<R<Z, S<Ternary, S<LessOrEquals, S<Product, U<3, 3>, U<3, 3>>, U<3, 1>>, S<N, U<3, 3>>, U<3, 3>>>>;

using Sqrt = S<Ternary, S<Less, Id, Two>, IntermediateSqrt, S<LimitedDecrement , IntermediateSqrt>>;
/*
 * Sqrt(a) = {max p | p * p <= a}
 *
 * R<>(9, 0) = 0
 * R<>(9, 1) = Ternary(LessOrEquals(Product(1, 1), 9), 1, 0) = 1
 * R<>(9, 2) = Ternary(LessOrEquals(Product(2, 2), 9), 2, 1) = 2
 * R<>(9, 3) = Ternary(LessOrEquals(Product(3, 3), 9), 3, 2) = 3
 * R<>(9, 4) = Ternary(LessOrEquals(Product(4, 4), 9), 4, 3) = 3
 * etc... same for 5, 6, ..., 9
 */

using BitAnd = Z; // TODO
using BitXor = Z; // TODO
using First = Z; // TODO
using NthPrime = Z; // TODO
using Pair = Z; // TODO; Pair is a bijective function: N0 x N0 -> N0
using PairGetLeft = Z; // TODO; retrieves the left number 'a' from the Pair(a, b)
using PairGetRight = Z; // TODO; retrieves the right number 'b' from the Pair(a, b)

using Nil = One;
// Gödel number of an empty list

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
    std::cout << "=== LimitedDifference ===" << "\n";
    printPRF(LimitedDifference(), "LimitedDifference", NatArgs{8, 6});
    printPRF(LimitedDifference(), "LimitedDifference", NatArgs{8, 8});
    printPRF(LimitedDifference(), "LimitedDifference", NatArgs{6, 8});
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
    std::cout << "=== Div ===" << "\n";
    printPRF(Div(), "Div", NatArgs{5, 2});
    printPRF(Div(), "Div", NatArgs{2, 5});
    printPRF(Div(), "Div", NatArgs{5, 5});
    printPRF(Div(), "Div", NatArgs{49, 5});
    printPRF(Div(), "Div", NatArgs{50, 5});
    printPRF(Div(), "Div", NatArgs{51, 5});
    printPRF(Div(), "Div", NatArgs{0, 5});
    printPRF(Div(), "Div", NatArgs{5, 0});
    printPRF(Div(), "Div", NatArgs{0, 0});
    printPRF(Div(), "Div", NatArgs{3, 1});
    std::cout << "=== Mod ===" << "\n";
    printPRF(Mod(), "Mod", NatArgs{5, 2});
    printPRF(Mod(), "Mod", NatArgs{2, 5});
    printPRF(Mod(), "Mod", NatArgs{5, 5});
    printPRF(Mod(), "Mod", NatArgs{49, 5});
    printPRF(Mod(), "Mod", NatArgs{50, 5});
    printPRF(Mod(), "Mod", NatArgs{51, 5});
    printPRF(Mod(), "Mod", NatArgs{0, 5});
    printPRF(Mod(), "Mod", NatArgs{5, 0});
    printPRF(Mod(), "Mod", NatArgs{0, 0});
    printPRF(Mod(), "Mod", NatArgs{3, 1});
    std::cout << "=== IsPrime ===" << "\n";
    printPRF(IsPrime(), "IsPrime", NatArgs{2});
    printPRF(IsPrime(), "IsPrime", NatArgs{3});
    printPRF(IsPrime(), "IsPrime", NatArgs{4});
    printPRF(IsPrime(), "IsPrime", NatArgs{5});
    printPRF(IsPrime(), "IsPrime", NatArgs{6});
    printPRF(IsPrime(), "IsPrime", NatArgs{7});
    printPRF(IsPrime(), "IsPrime", NatArgs{8});
    printPRF(IsPrime(), "IsPrime", NatArgs{9});
    printPRF(IsPrime(), "IsPrime", NatArgs{10});
    printPRF(IsPrime(), "IsPrime", NatArgs{11});
    std::cout << "=== Plog ===" << "\n";
    printPRF(Plog(), "Plog", NatArgs{6, 72});
    printPRF(Plog(), "Plog", NatArgs{3, 17});
    printPRF(Plog(), "Plog", NatArgs{3, 18});
    printPRF(Plog(), "Plog", NatArgs{3, 19});
    std::cout << "=== Sqrt ===" << "\n";
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{0});
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{1});
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{2});
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{3});
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{4});
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{5});
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{6});
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{7});
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{8});
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{9});
    printPRF(Sqrt(), "IntermediateSqrt", NatArgs{10});

    return 0;
}