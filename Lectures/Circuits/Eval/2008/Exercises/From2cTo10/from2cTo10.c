// This program converts from 2-complement binary to decimal

// Standard librairies
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM  -1
#define NO_BIN   -2

// Computation
//
int from2cTo10__ (int dec, const char bin[]) {
  return *bin ? from2cTo10__(dec*2+*bin-'0',bin+1) : dec;
}

int from2cTo10 (const char bin[]) {
  return from2cTo10__('0'-*bin,bin+1);
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
      printf("%d\n",from2cTo10(argv[1]));
  } else ret_code = ARG_NUM;

  if (ret_code) err_msg(ret_code);
  
  return ret_code;
}

