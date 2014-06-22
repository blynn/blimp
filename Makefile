CC:=gcc
#CFLAGS:=-std=gnu99 -Wall -O2
CFLAGS:=-std=gnu99 -Wall -g

blimp : blimp.c ; $(CC) $(CFLAGS) -o $@ $^ ~/z/blt/blt.c -I ~/z/blt/ -lreadline -lgmp
