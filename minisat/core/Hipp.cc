
#include "core/Hipp.h"
extern Solver* solver;

bool Hipp::load(gzFile file) {
    StreamBuffer in(file);
    size_pop = 0;
    for(;;) {
        skipWhitespace(in);
        if(*in == EOF) break;
        else if(*in == 's') {
            eagerMatch(in, "s ");
            size_gen = parseInt(in);
        }
        else {
            pop.growTo(size_gen * (size_pop + 1));
            for(int i = 0; i < size_gen; ++i)
                pop[size_gen*size_pop + i] = parseInt(in);
            size_pop++;
        }
    }
    return true;
}

Lit mlit(Var v, bool val) {
    return mkLit(v,!val);
}

/* k is the id of the haplotype, 0 <= k < r
 * i is the id of the genome, 0 <= i < size_pop
 * r*size_gen <= s < r*size_gen + 2*r*size_pop
 */
Lit Hipp::s(ST t, int k, int i, bool b) {
    int l = 0;
    if(t == Father) l = 1;
    int n = r * size_gen // offset to start after h
          + 2 * i * r    // selecting the genome
          + l * r        // offset for mother
          + k;           // the haplotype
    return mlit(n,b);
}

/* k is the id of the haplotype, 0 <= k < r
 * i is the location 0 <= i < size_gen
 * 0 <= h(k,i,b) < r*size_gen
 */
Lit Hipp::h(int k, int i, bool b) {
    int n = k * size_gen + i;
    return mlit(n,b);
}

Solver* Hipp::createSolver() {
    int mx = 2*r*size_pop + r*size_gen;
    Solver* S = new Solver(r, size_gen, size_pop);

    /* Creating the variables */
    while(S->nVars() < mx)
        S->newVar();

    /*  Creating the haplotype clauses */
    for(int j = 0; j < size_pop; ++j) {
        for(int i = 0; i < size_gen; ++i) {
            for(int k = 0; k < r; ++k) {
                if(pop[j*size_gen + i] == 0) {
                    S->addClause(h(k,i,false), s(Father,k,j,false));
                    S->addClause(h(k,i,false), s(Mother,k,j,false));
                } else if(pop[j*size_gen + i] == 1) {
                    S->addClause(h(k,i,true), s(Father,k,j,false));
                    S->addClause(h(k,i,true), s(Mother,k,j,false));
                } else {
                    Var n1 = S->newVar();
                    Var n2 = S->newVar();
                    S->addClause(mlit(n1,true),mlit(n2,true));
                    S->addClause(mlit(n1,false),mlit(n2,false));
                    S->addClause(h(k,i,true),s(Father,k,j,false),mlit(n1,true));
                    S->addClause(h(k,i,false),s(Father,k,j,false),mlit(n1,false));
                    S->addClause(h(k,i,true),s(Mother,k,j,false),mlit(n2,true));
                    S->addClause(h(k,i,false),s(Mother,k,j,false),mlit(n2,false));
                }
            }
        }
    }

    /* Creating the selection clauses */
    for(int i = 0; i < size_pop; ++i) {
        vec<Lit> v1;
        vec<Lit> v2;
        v1.growTo(r);
        v2.growTo(r);
        for(int k = 0; k < r; ++k) {
            v1[k] = s(Father,k,i,true);
            v2[k] = s(Mother,k,i,true);
        }
        S->addClause(v1);
        S->addClause(v2);
    }

    return S;
}

std::vector<int> Hipp::extract(Solver* S) {
    std::vector<int> sol;
    sol.resize(r*size_gen);
    for(int k = 0; k < r; ++k) {
        for(int i = 0; i < size_gen; ++i) {
            lbool l = S->modelValue(h(k,i,true));
            if(l == l_True) {
                sol[k*size_gen + i] = 1;
            } else if(l == l_False) {
                sol[k*size_gen + i] = 0;
            } else {
                sol[k*size_gen + i] = -1;
            }
        }
    }
    return sol;
}
        
std::vector<int> Hipp::solve() {
    for(r = 1;; ++r) {
        fprintf(stdout, "Trying %i haplotypes\n", r);
        Solver* S = createSolver();
        fprintf(stdout, "System with %i clauses and %i variables.\n", S->nClauses(), S->nVars());
        S->verbosity = 0;
        if(S->simplify()) {
            bool ret = S->solve();
            if(ret)
                return extract(S);
        }
        delete S;
    }
}

int Hipp::size_genome() {
    return size_gen;
}

