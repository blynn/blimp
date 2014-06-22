CC:=gcc
#CFLAGS:=-std=gnu99 -Wall -O2
CFLAGS:=-std=gnu99 -Wall -g

bl : bl.c ; $(CC) $(CFLAGS) -o $@ bl.c ~/z/blt/blt.c -I ~/z/blt/ -lreadline -lgmp
