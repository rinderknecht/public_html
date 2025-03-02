/* compute_result.c */

#include "tools.h"
#include "compute_result.h"

compute_struct_t* compute_result(int num)
{
  compute_struct_t* res = NEW(compute_struct_t);
  res->q = num / 3;
  res->r = num % 3;
  return res;
}
