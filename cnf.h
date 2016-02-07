
#ifndef DEF_CNF
#define DEF_CNF

#include "bool.h"
#include "list.h"

typedef struct _cnf cnf_t;
typedef struct _clause clause_t;
typedef struct _var {
    uint32_t id;
    bool value;
} var_t;
typedef struct _implication {
    var_t v1;
    var_t v2;
    var_t bond;
} implication_t;

/* Create clauses */
clause_t* cnf_clause1(var_t v1);
clause_t* cnf_clause2(var_t v1, var_t v2);
clause_t* cnf_clause3(var_t v1, var_t v2, var_t v3);
/* Free a clause and set the pointer to NULL */
void cnf_free_clause(clause_t** clause);

/* Create cnf, with the number of variables */
cnf_t* cnf_create(int max);
/* Free a cnf and set the pointer to NULL */
void cnf_free(cnf_t** cnf);
/* Add a clause to the cnf. The pointer will now be owned by the cnf structure,
 * and free'd with it */
void cnf_add_clause(cnf_t* cnf, clause_t* clause);
/* Clear assumptions pile and status */
void cnf_unset(cnf_t* cnf);
/* Suppose a var set to a value */
void cnf_set_var(cnf_t* cnf, var_t var);
/* Return implications (if v1 or v2 id is 0, it means there is no variable) */
list_t* cnf_implications(cnf_t* cnf);
/* Save status of assumptions */
void cnf_push_assum(cnf_t* cnf, uint32_t id);
/* Go back to a previous level of assumptions */
void cnf_rewind_assum(cnf_t* cnf, uint32_t id);

#endif//DEF_CNF

