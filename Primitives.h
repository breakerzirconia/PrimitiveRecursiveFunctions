//
// Created by T3RCH1K and Nik Carlson
//

#ifndef MATHLOGICHW7_PRIMITIVES_H
#define MATHLOGICHW7_PRIMITIVES_H

#include <iostream>
#include <vector>
#include <cassert>

using NatArgs = std::vector<unsigned long long>;

struct Z {
    static unsigned long long compute(const NatArgs& x) {
        assert(x.size() == 1);
        return 0;
    }
};

struct N {
    static unsigned long long compute(const NatArgs& x) {
        assert(x.size() == 1);
        return x[0] + 1;
    }
};

template<unsigned n, unsigned i>
struct U {
    static unsigned long long compute(const NatArgs& x) {
        assert(x.size() == n);
        return x[i - 1];
    }
};

template<typename F, typename... G>
struct S {
    static unsigned long long compute(const NatArgs& x) {
        NatArgs g;
        (g.push_back(G::compute(x)), ...);
        return F::compute(g);
    }
};

template<typename F, typename G>
struct R {
    static unsigned long long compute(const NatArgs& xy) {
        unsigned y = *(xy.end() - 1);
        NatArgs xy0(xy.begin(), xy.end() - 1);
        if (y == 0) {
            return F::compute(xy0);
        }
        xy0.push_back(y - 1);
        xy0.push_back(R<F, G>::compute(xy0));
        return G::compute(xy0);
    }
};

#endif //MATHLOGICHW7_PRIMITIVES_H
