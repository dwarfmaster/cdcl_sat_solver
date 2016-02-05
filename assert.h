
#ifndef DEF_ASSERT
#define DEF_ASSERT

#include <stdlib.h>
#include <stdio.h>

#define ASSERT(a) if(!(a)) { printf("Abort on %s:%d\n", __FILE__, __LINE__); \
                             exit(EXIT_FAILURE); }

#endif

