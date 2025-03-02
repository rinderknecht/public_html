// This program converts from decimal to binary

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM    -1
#define DEC_DIGITS -2

// Computation
//
char* from10to2 (unsigned int dec) {
  int i = -1;
  unsigned int in = dec; // Copy

  // Computing the number of bits
  //
  do {
    dec /= 2;
    i++;
  } while (dec);
  // Here, i+1 is the number of bits.

  // Allocating without checking for memory overflow
  //
  char* bin = (char*) malloc((i+2)*sizeof(char));
  bin[i+1] = '\0';

  // Computing the bits
  //
  do {
    bin[i] = in % 2 + '0';
    in /= 2;
    i--;
  } while (in);

  return bin;
}

// Printing error messages
//
void err_msg (int err_code) {
  switch (err_code) {
  case DEC_DIGITS:
    fprintf(stderr,"error: only decimal digits allowed\n");
    break;
  case ARG_NUM:
    fprintf(stderr,"error: exactly one argument allowed\n");
    break;
  }
  fflush(stderr);
}

// Main Input/Output
//
int main(int argc, char** argv) {
  int ret_code = 0;

  if (argc == 2) {
    unsigned int dec = 0;
    char* arg = argv[1];

    while ('0' <= *arg && *arg <= '9') {
      dec = 10 * dec + (*arg - '0');
      arg++;
    }

    if (*arg)
      ret_code = DEC_DIGITS;
    else
      if (dec) {
        char* bin = from10to2(dec);
        puts(bin);
        free(bin);
      } else puts("0");
  } else ret_code = ARG_NUM;
  
  if (ret_code) err_msg(ret_code);

  return ret_code;
}
