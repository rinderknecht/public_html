// This program converts from binary to hexadecimal

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM  -1
#define NO_BIN   -2

// Making an hexadecimal digit from the `len' first bits
// of `bin', where `len' must range between 1 and 4.
//
char mk_digit(const char bin[], unsigned int len) {
  unsigned int dec = 0;

  for(int i = 0; i < len; i++)
    dec = dec*2 + (bin[i] - '0');

  return dec < 10 ? dec + '0' : dec - 10 + 'A';
}

char* from2to16 (const char bin[], unsigned int b_len) {
  // The number of remaining leftmost bits after grouping
  // four by four, from right to left. (Zero if the total
  // number of bits is a multiple of four.)
  //
  unsigned int h_rem = b_len % 4;

  // Length of the hexadecimal string
  //
  unsigned int h_len = b_len/4 + (h_rem ? 1 : 0);

  // Allocating without checking for memory overflow
  //
  char* hex = (char*) malloc((h_len+1) * sizeof(char));
  hex[h_len] = '\0';

  // The leftmost hexadecimal digit if `h_rem' is not zero
  //
  unsigned int i = 0;
 
  if (h_rem) {
    hex[0] = mk_digit(bin,h_rem);
    bin = bin + h_rem;
    i++;
  }

  // The remaining hexadecimal digits
  //
  for (; i < h_len; i++) {
    hex[i] = mk_digit(bin,4);
    bin = bin + 4;
  }

  return hex;
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

    // Let us skip the leftmost zeros.
    //
    while(*arg == '0') arg++;

    // The remainder must be a bit string.
    //
    unsigned int idx = 0;
    while (arg[idx] == '0' || arg[idx] == '1') idx++;

    if (arg[idx])
      ret_code = NO_BIN;
    else     
      if (idx) { // The input is NOT only made of zeros.
        char* hex = from2to16(arg,idx);
        puts(hex);
        free(hex);
      }
      else puts("0");
  } else ret_code = ARG_NUM;

  if (ret_code) err_msg(ret_code);
  
  return ret_code;
}
