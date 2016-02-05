
#ifndef DEF_LIST
#define DEF_LIST

#include "bool.h"

typedef struct _list list_t;

list_t* list_create(void (*freeer)(void*));
/* If freeer was not NULL, will free the data */
void list_free(list_t* l);
/* Will the next elements of the tail without copying them. They can be free'd,
 * as a pointer count will be kept. */
list_t* list_tail(list_t* l);
void* list_head(list_t* l);
bool list_empty(list_t* l);
/* Add an element on top */
void list_add(list_t* l, void* elem);
/* Remove the element on top */
void list_rm(list_t* l);
uint32_t list_size(list_t* l);

#endif//DEF_LIST

