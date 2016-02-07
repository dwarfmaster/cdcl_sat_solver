
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
    bool freeel; /* Should the elems be free'd */
};

list_t* list_create(void (*freeer)(void*)) {
    struct _list* l = malloc(sizeof(struct _list));
    ASSERT(l != NULL);

    l->first  = NULL;
    l->freeer = freeer;
    l->size   = 0;
    l->freeel = true;
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
    if(l->freeel) {
        free_elem(l->first, l->freeer);
    }
    free(l);
}

list_t* list_tail(list_t* l) {
    ASSERT(l != NULL);
    ASSERT(l->first != NULL);
    struct _list* tl = malloc(sizeof(struct _list));
    ASSERT(tl != NULL);

    tl->size   = l->size - 1;
    tl->freeer = l->freeer;
    tl->first  = l->first->next;
    tl->freeel = false;
    return tl;
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

