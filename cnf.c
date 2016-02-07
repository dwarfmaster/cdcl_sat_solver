
#include "cnf.h"
#include "assert.h"

#include <stdlib.h>

struct _clause1 {
    var_t v1;
};
struct _clause2 {
    var_t v1;
    var_t v2;
};
struct _clause3 {
    var_t v1;
    var_t v2;
    var_t v3;
};
union _clauses {
    struct _clause1 c1;
    struct _clause2 c2;
    struct _clause3 c3;
};

struct _clause {
    uint8_t type; /* 1, 2 or 3 */
    union _clauses c;
};

struct _assum_pile {
    tribool* a;
    uint32_t id;
};

struct _cnf {
    list_t* clauses;
    list_t* assums;
    tribool* assum;
    uint32_t nb;
};

clause_t* cnf_clause1(var_t v1) {
    struct _clause* c = malloc(sizeof(struct _clause));
    ASSERT(c != NULL);
    c->type = 1;
    c->c.c1.v1 = v1;
    return c;
}

clause_t* cnf_clause2(var_t v1, var_t v2) {
    struct _clause* c = malloc(sizeof(struct _clause));
    ASSERT(c != NULL);
    c->type = 2;
    c->c.c2.v1 = v1;
    c->c.c2.v2 = v2;
    return c;
}

clause_t* cnf_clause3(var_t v1, var_t v2, var_t v3) {
    struct _clause* c = malloc(sizeof(struct _clause));
    ASSERT(c != NULL);
    c->type = 3;
    c->c.c3.v1 = v1;
    c->c.c3.v2 = v2;
    c->c.c3.v3 = v3;
    return c;
}

void cnf_free_clause(clause_t** clause) {
    ASSERT(clause != NULL);
    ASSERT(*clause != NULL);
    free(*clause);
    *clause = NULL;
}

static void free_clause(void* c) {
    ASSERT(c != NULL);
    void* p = c;
    cnf_free_clause(p);
}

static void free_assum_pile(void* c) {
    ASSERT(c != NULL);
    struct _assum_pile* p = c;
    free(p->a);
    free(p);
}

cnf_t* cnf_create(int max) {
    size_t i;
    ASSERT(max > 0);
    struct _cnf* cnf = malloc(sizeof(struct _cnf));
    ASSERT(cnf != NULL);
    cnf->nb      = max;
    cnf->assums  = list_create(free_assum_pile);
    cnf->clauses = list_create(free_clause);
    cnf->assum   = malloc(sizeof(tribool) * max);
    for(i = 0; i < cnf->nb; ++i) {
        cnf->assum[i] = unset;
    }
    return cnf;
}

void cnf_free(cnf_t** cnf) {
    ASSERT(cnf != NULL);
    ASSERT(*cnf != NULL);
    list_free((*cnf)->assums);
    list_free((*cnf)->clauses);
    free((*cnf)->assum);
    free(*cnf);
    *cnf = NULL;
}

void cnf_add_clause(cnf_t* cnf, clause_t* clause) {
    ASSERT(cnf != NULL);
    ASSERT(clause != NULL);
    list_add(cnf->clauses, clause);
}

void cnf_unset(cnf_t* cnf) {
    ASSERT(cnf != NULL);
    size_t i;
    list_free(cnf->assums);
    cnf->assums = list_create(free_assum_pile);
    for(i = 0; i < cnf->nb; ++i) {
        cnf->assum[i] = unset;
    }
}

void cnf_set_var(cnf_t* cnf, var_t var) {
    ASSERT(cnf != NULL);
    cnf->assum[var.id] = var.value;
}

static implication_t* create_implication(var_t v1, var_t v2, var_t imp) {
    implication_t* i = malloc(sizeof(implication_t));
    ASSERT(i != NULL);
    i->v1   = v1;
    i->v2   = v2;
    i->bond = imp;
    return i;
}

static void handle_c2(list_t* imp, cnf_t* cnf, var_t v1, var_t v2) {
    if(cnf->assum[v1.id] != unset && cnf->assum[v1.id] != v1.value) {
        list_add(imp, create_implication(
                    (var_t){.id = v1.id, .value = !v1.value},
                    (var_t){.id = 0},
                    v2));
    }
}

static void handle_c3(list_t* imp, cnf_t* cnf, var_t v1, var_t v2, var_t v3) {
    if(cnf->assum[v1.id] != unset && cnf->assum[v1.id] != v1.value) {
        handle_c2(imp, cnf, v2, v3);
        handle_c2(imp, cnf, v3, v2);
    }
}

list_t* cnf_implications(cnf_t* cnf) {
    ASSERT(cnf != NULL);
    list_iterator_t it;
    list_t* imp = list_create(free);
    ASSERT(imp != NULL);

    for(it = list_begin(cnf->clauses);
            it != list_end(cnf->clauses);
            it = list_next(it)) {
        struct _clause* cl = list_it_data(it);
        switch(cl->type) {
            case 1:
                if(cnf->assum[cl->c.c1.v1.id] == unset) {
                    list_add(imp, create_implication((var_t){.id = 0},
                                                     (var_t){.id = 0},
                                                     cl->c.c1.v1));
                }
                break;
            case 2:
                handle_c2(imp, cnf, cl->c.c2.v1, cl->c.c2.v2);
                handle_c2(imp, cnf, cl->c.c2.v2, cl->c.c2.v1);
                break;
            case 3:
                handle_c3(imp, cnf, cl->c.c3.v1, cl->c.c3.v2, cl->c.c3.v3);
                handle_c3(imp, cnf, cl->c.c3.v2, cl->c.c3.v3, cl->c.c3.v1);
                handle_c3(imp, cnf, cl->c.c3.v3, cl->c.c3.v1, cl->c.c3.v2);
                break;
            default:
                ASSERT(false);
        }
    }

    return imp;
}

void cnf_push_assum(cnf_t* cnf, uint32_t id) {
    size_t i;
    ASSERT(cnf != NULL);
    struct _assum_pile* p = malloc(sizeof(struct _assum_pile));
    ASSERT(p != NULL);
    p->id = id;
    p->a  = cnf->assum;
    cnf->assum = malloc(sizeof(tribool) * cnf->nb);
    for(i = 0; i < cnf->nb; ++i) {
        cnf->assum[i] = p->a[i];
    }
    list_add(cnf->assums, p);
}

void cnf_rewind_assum(cnf_t* cnf, uint32_t id) {
    ASSERT(cnf != NULL);
    size_t i;
    bool cont = true;
    while(!(list_empty(cnf->assums)) && cont) {
        struct _assum_pile* p = list_head(cnf->assums);
        if(p->id == id) {
            cont = false;
        }
        list_rm(cnf->assums);
    }
    if(list_empty(cnf->assums)) {
        for(i = 0; i < cnf->nb; ++i) {
            cnf->assum[i] = unset;
        }
    } else {
        struct _assum_pile* p = list_head(cnf->assums);
        for(i = 0; i < cnf->nb; ++i) {
            cnf->assum[i] = p->a[i];
        }
    }
}

