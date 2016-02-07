
#include "list.h"
#include "assert.h"

#include <stdlib.h>

struct _elem {
    void* data;
    struct _elem* next;
};

struct _list {
    struct _elem* first;
    uint32_t size;
    void (*freeer)(void*);
};

list_t* list_create(void (*freeer)(void*)) {
    struct _list* l = malloc(sizeof(struct _list));
    ASSERT(l != NULL);

    l->first  = NULL;
    l->freeer = freeer;
    l->size   = 0;
    return l;
}

static void free_elem(struct _elem* e, void (*freeer)(void*)) {
    if(e == NULL) return;
    free_elem(e->next, freeer);
    if(freeer != NULL) {
        freeer(e->data);
    }
    free(e);
}

void list_free(list_t* l) {
    ASSERT(l != NULL);
    free_elem(l->first, l->freeer);
    free(l);
}

void* list_head(list_t* l) {
    ASSERT(l != NULL);
    ASSERT(l->first != NULL);
    return l->first->data;
}

bool list_empty(list_t* l) {
    ASSERT(l != NULL);
    return l->first == NULL;
}

void list_add(list_t* l, void* elem) {
    ASSERT(l != NULL);
    struct _elem* el = malloc(sizeof(struct _elem));
    el->next  = l->first;
    el->data  = elem;
    l->first  = el;
    l->size++;
}

void list_rm(list_t* l) {
    ASSERT(l != NULL);
    ASSERT(l->first != NULL);
    struct _elem* e = l->first;
    l->first = e->next;
    l->size--;
    if(l->freeer != NULL) {
        l->freeer(e->data);
    }
    free(e);
}

uint32_t list_size(list_t* l) {
    ASSERT(l != NULL);
    return l->size;
}

list_iterator_t list_begin(list_t* l) {
    ASSERT(l != NULL);
    return l->first;
}

list_iterator_t list_next(list_iterator_t it) {
    if(it == NULL)
        return NULL;
    else
        return ((struct _elem*)it)->next;
}

void* list_it_data(list_iterator_t it) {
    if(it == NULL)
        return NULL;
    else
        return ((struct _elem*)it)->data;
}

list_iterator_t list_end(list_t* l) {
    if(l) { } /* Avoid warnings */
    return NULL;
}
