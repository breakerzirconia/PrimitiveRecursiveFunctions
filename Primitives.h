//
// Created by T3RCH1K and Nik Carlson
//

#ifndef MATHLOGICHW7_PRIMITIVES_H
#define MATHLOGICHW7_PRIMITIVES_H

#include <iostream>
#include <vector>
#include <cassert>

struct Z {
    static unsigned compute(const std::vector<unsigned>& x) {
        assert(x.size() == 1);
        return 0;
    }
};

struct N {
    static unsigned compute(const std::vector<unsigned>& x) {
        assert(x.size() == 1);
        return x[0] + 1;
    }
};

template<unsigned n, unsigned i>
struct U {
    static unsigned compute(const std::vector<unsigned>& x) {
        assert(x.size() == n);
        return x[i - 1];
    }
};

template<typename F, typename... G>
struct S {
    static unsigned compute(const std::vector<unsigned>& x) {
        std::vector<unsigned> g;
        (g.push_back(G::compute(x)), ...);
        return F::compute(g);
    }
};

template<typename F, typename G>
struct R {
    static unsigned compute(const std::vector<unsigned>& xy) {
        unsigned y = *(xy.end() - 1);
        std::vector<unsigned> xy0(xy.begin(), xy.end() - 1);
        if (y == 0) {
            return F::compute(xy0);
        }
        xy0.push_back(y - 1);
        xy0.push_back(R<F, G>::compute(xy0));
        return G::compute(xy0);
    }
};

#endif //MATHLOGICHW7_PRIMITIVES_H
