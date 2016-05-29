#ifndef DEF_HIPP
#define DEF_HIPP

#include <stdio.h>
#include <vector>
#include "utils/ParseUtils.h"
#include "core/SolverTypes.h"
#include "core/Solver.h"

using namespace Minisat;

class Hipp {
    public:
        Hipp() = default;
        bool load(gzFile file);
        std::vector<int> solve();
        int size_genome();

    private:
        vec<int> pop;
        int size_pop;
        int size_gen;

        int r;
        enum ST {Father,Mother};
        Lit s(ST t, int k, int i, bool b);
        Lit h(int k, int i, bool b);
        Solver* createSolver();
        std::vector<int> extract(Solver* S);
};

#endif

