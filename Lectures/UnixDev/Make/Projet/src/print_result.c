/* print_result.c */

#include <stdio.h>
#include "print_result.h"

void print_result(const compute_struct_t* c)
{
  printf("%d %d\n", c->q, c->r);
}
