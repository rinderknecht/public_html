
CC = gcc
CFLAGS = -Wall

.PHONY: re clean

all: main

compute_result.o: compute_result.c compute_result.h compute_struct.h tools.h
	$(CC) $(CFLAGS) -c compute_result.c

parse_num.o: parse_num.c parse_num.h tools.h
	$(CC) $(CFLAGS) -c parse_num.c

print_result.o: print_result.c print_result.h compute_struct.h tools.h
	$(CC) $(CFLAGS) -c print_result.c

main.o: main.c print_result.h compute_result.h parse_num.h
	$(CC) $(CFLAGS) -c main.c

main: main.o print_result.o parse_num.o compute_result.o
	$(CC) -o main print_result.o parse_num.o compute_result.o main.o

clean:
	rm -f main *.o *~ \#*

re: clean main
