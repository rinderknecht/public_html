// Incrementing a binary number with overflows disallowed

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM   -1
#define NO_BIN    -2
#define OVERFLOW  -3

// Computation
//
char* incr2 (const char bin[], unsigned int i) {
  unsigned int carry = 1;

  char* incr = (char*) malloc(sizeof(char)*(i+1));
  incr[i] = '\0';

  do {
    i--;
    incr[i] = bin[i] + carry;
    if (incr[i] == '2') incr[i] = '0'; else carry = 0;
  } while (i);
  
  if (carry) { free(incr); incr = NULL; }

  return incr;
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
  case OVERFLOW:
    fprintf(stderr,"error: overflow.\n"); break;
  }
  fflush(stderr);
}

// Main Input/Output
//
int main(int argc, char** argv) {
  int ret_code = 0;

  if (argc == 2) {
    char* arg = argv[1];
    unsigned int i = 0;

    while (arg[i] == '0' || arg[i] == '1') i++;

    if (arg[i])
      ret_code = NO_BIN;
    else {
      char* incr = incr2(arg,i);
      if (incr) { puts(incr); free(incr); }
      else ret_code = OVERFLOW;
    }
  } else ret_code = ARG_NUM;

  if (ret_code) err_msg(ret_code);
  
  return ret_code;
}
