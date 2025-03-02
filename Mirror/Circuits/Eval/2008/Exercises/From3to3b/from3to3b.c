// This program converts from ternary to balanced ternary

//  carry in   0 0 0 1 1 1
//  trit       0 1 2 0 1 2
//  ----------------------
//  btrit      0 1 I 1 I 0
//  carry out  0 0 1 0 1 1
//
// 111...12... -> ternary longer by one trit

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM  -1
#define NO_TER   -2

// Computation
//
char* from3to3b (const char ter[], unsigned int i) {
  // Computing the length of the result
  //
  unsigned int j = 0;

  while (ter[j] == '1') j++;

  j = i + (ter[j] == '2'? 1 : 0);

  // Allocating without checking for memory overflow
  //
  char* bter = (char*) malloc((j+1) * sizeof(char));
  bter[j] = '\0';

  // Computing the trits
  //
  unsigned int carry = 0;

  do {
    i--; j--;
    switch(ter[i] + carry) {
    case '0': bter[j] = '0'; break;
    case '1': bter[j] = '1'; carry = 0; break;
    case '2': bter[j] = 'I'; carry = 1; break;
    case '3': bter[j] = '0'; break;
    }
  } while (0 < i);

  if (carry) bter[0] = '1';

  return bter;
}

// Printing error messages
//
void err_msg (int err_code) {
  switch (err_code) {
  case ARG_NUM:
    fprintf(stderr,"error: exactly one argument allowed\n");
    break;
  case NO_TER:
    fprintf(stderr,"error: only trits are allowed\n");
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

    // Let us skip the leftmost zeros.
    //
    while(*arg == '0') arg++;

    // The remainder must be a string.
    //
    unsigned int idx = 0;
    while (arg[idx] == '0' || arg[idx] == '1' || arg[idx] == '2') idx++;

    if (arg[idx])
      ret_code = NO_TER;
    else     
      if (idx) { // The input is NOT only made of zeros.
        char* bter = from3to3b(arg,idx);
        puts(bter);
        free(bter);
      }
      else puts("0");
  } else ret_code = ARG_NUM;

  if (ret_code) err_msg(ret_code);
  
  return ret_code;
}
