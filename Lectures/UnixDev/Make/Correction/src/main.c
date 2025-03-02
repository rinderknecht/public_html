/* main.c */

#include <stdlib.h>
#include <stdio.h>

#include "parse_num.h"
#include "compute_result.h"
#include "print_result.h"

int main(int argc, char **argv)
{
  int num;

  /* check arguments. */
  if (argc < 1 + 1)
    {
      printf("Erreur: on attend un nombre pour argument.\n");
      exit(EXIT_FAILURE);
    }
  
  /* parse arguments. */
  if (! parse_num(argv[1], &num))
    {
      fprintf(stderr, "Erreur: veuillez spécifier un nombre.\n");
      exit(EXIT_FAILURE);
    }
  /* compute the result. */
  print_result(compute_result(num));
  exit(EXIT_SUCCESS);
}

