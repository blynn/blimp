CC:=gcc
#CFLAGS:=-std=gnu99 -Wall -O2
CFLAGS:=-std=gnu99 -Wall -g

blimp : blimp.c ; $(CC) $(CFLAGS) -o $@ $^ blt.c -lreadline -lgmp

test: blimp ; go test blimp_test.go
