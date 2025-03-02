// This program converts from binary to decimal

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM  -1
#define NO_BIN   -2

// Computation
//
unsigned int from2to10 (const char bin[]) {
  unsigned int dec = 0;

  for( ; *bin; bin++)
    dec = dec*2 + (*bin - '0');

  return dec;
}

// Printing error messages
//
void err_msg (int err_code) {
  switch (err_code) {
  case ARG_NUM:
    fprintf(stderr,"error: exactly one argument allowed\n");
    break;
  case NO_BIN:
    fprintf(stderr,"error: only bits are allowed\n");
    break;
  }
  fflush(stderr);
}

// Main Input/Output
//
int main(int argc, char** argv) {
  int ret_code = 0;

  if (argc == 2) {
    char* arg = argv[1];

    while (*arg == '0' || *arg == '1') arg++;

    if (*arg)
      ret_code = NO_BIN;
    else
      printf("%u\n",from2to10(argv[1]));
  } else ret_code = ARG_NUM;

  if (ret_code) err_msg(ret_code);
  
  return ret_code;
}
