CC:=gcc
CFLAGS:=-std=gnu99 -Wall -O2

blimp : blimp.c ; $(CC) $(CFLAGS) -o $@ $^ blt.c -lreadline -lgmp

test: blimp ; go test blimp_test.go
