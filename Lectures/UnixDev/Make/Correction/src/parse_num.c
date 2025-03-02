/* parse_num.c */

#include <stdlib.h>
#include "parse_num.h"
#include "tools.h"

int parse_num(const char* arg, int* res)
{
  char* endptr;
  *res = strtol(arg, &endptr, 10);
  if (endptr == arg)
    return FALSE;
  return TRUE;
}
