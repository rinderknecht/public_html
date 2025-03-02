// Standard libraries
//
#include<stdio.h>
#include<stdlib.h>

typedef int (*fp)();

int fact(fp f, int n) {
  return n? n * ((int (*)(fp,int))f)(f,n-1) : 1;
}

int read(int dec, char arg[]) {
  return ('0' <= *arg && *arg <= '9')? 
           read(10*dec+(*arg - '0'),arg+1)
         : dec;
}

// Main Input/Output
//
int main(int argc, char** argv) {
  if (argc == 2) {
    printf("%u\n",fact(&fact,read(0,argv[1])));
  }
  else printf("Only one integer allowed.\n");
}
