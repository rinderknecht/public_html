// display.c

#include "../config.h"
#include <stdlib.h>
#include <errno.h>
#include <error.h>
#include <unistd.h>

#ifdef HAVE_LIBNCURSES
# include <curses.h>

void display_message()
{
  int i, x, y;

#ifdef HAVE_LIBNOTPRESENT
  char* message = "Hello World with omega !";
#else
  char* message = "Hello World";
#endif // HAVELIBNOTPRESENT
  WINDOW* win = initscr();
  if (win == NULL)
    error(EXIT_FAILURE, errno, "Initialization problem.");
  y=LINES/2;
  x=(COLS-strlen(message))/2;
  mvprintw(y,x,message);
  refresh();
  sleep(2);
  (void)clear();
  (void)refresh();
  (void)endwin();
}
#else 
# include <stdio.h>
void display_message()
{
  printf("Hello world !\n");
}
#endif // HAVE_LIBNCURSES
