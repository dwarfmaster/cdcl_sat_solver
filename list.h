
#ifndef DEF_LIST
#define DEF_LIST

#include "bool.h"

typedef struct _list list_t;
typedef void* list_iterator_t;

/* All operation are constant time : O(1) */
list_t* list_create(void (*freeer)(void*));
/* If freeer was not NULL, will free the data */
void list_free(list_t* l);
void* list_head(list_t* l);
bool list_empty(list_t* l);
/* Add an element on top */
void list_add(list_t* l, void* elem);
/* Remove the element on top */
void list_rm(list_t* l);
uint32_t list_size(list_t* l);

/* The iterator remind valid as long as now destructive method is used on the
 * list */
list_iterator_t list_begin(list_t* l);
list_iterator_t list_next(list_iterator_t it);
void* list_it_data(list_iterator_t it);
list_iterator_t list_end(list_t* l);

#endif//DEF_LIST

