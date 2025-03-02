// This program converts from balanced ternary to ternary

//  carry in   0 0 0 I I I
//  trit       I 0 1 I 0 1
//  ----------------------
//  btrit      2 0 1 1 2 0
//  carry out  I 0 0 I I 0
//
// 1000I... or 1I... -> ternary shorter by one trit

// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

// Non-zero valued constants as error codes
//
#define ARG_NUM  -1
#define NO_BTER  -2
#define NEG      -3 

// Computation
//
char* from3bto3 (const char bter[], unsigned int i) {
  // Computing the length of the result
  //
  unsigned int j = 1;
  unsigned int carry = 0;

  while (bter[j] == '0') j++;
  j = i - (bter[j] =='I'? 1 : 0);

  // Allocating without checking for memory overflow
  //
  char* ter = (char*) malloc((j+1) * sizeof(char));
  ter[j] = '\0';

  // Starting again but saving the trits this time
  //
  carry = 0;
  int temp = 0;

  do {
    i--; j--;
    temp = (bter[i] == 'I'? -1 : bter[i] - '0') + carry;
    switch(temp) {
    case -2: ter[j] = '1';        carry = -1; break;
    case -1: ter[j] = '2';        carry = -1; break;
    default: ter[j] = temp + '0'; carry =  0;
    }
  } while (0 < j);

  return ter;
}

// Printing error messages
//
void err_msg (int err_code) {
  switch (err_code) {
  case ARG_NUM:
    fprintf(stderr,"error: exactly one argument allowed\n");
    break;
  case NO_BTER:
    fprintf(stderr,"error: only '0', '1' and 'I' are allowed\n");
    break;
  case NEG:
    fprintf(stderr,"error: no negative number allowed\n");
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

    if (*arg == 'I')
      ret_code = NEG;
    else {
      // The remainder must be a string of '0','1' and 'I'.
      //
      unsigned int idx = 0;
      while (arg[idx] == '0' || arg[idx] == '1' || arg[idx] == 'I') idx++;
      
      if (arg[idx])
        ret_code = NO_BTER;
      else
        if (idx) { // The input is NOT only made of zeros.
          char* ter = from3bto3(arg,idx);
          puts(ter);
          free(ter);
        }
        else puts("0");
    }
  } else ret_code = ARG_NUM;
  
  if (ret_code) err_msg(ret_code);
  
  return ret_code;
}
