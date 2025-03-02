// display.c

#include <stdlib.h>
#include <errno.h>
#include <curses.h>
#include <error.h>
#include <unistd.h>

void display_message()
{
  int i, x, y;
  char* message = "Hello World";
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
