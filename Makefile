.PHONY: clean
.DEFAULT_GOAL: 64-t.prg

64-t.prg: 64-t.o
	ld65 -o 64-t.prg -t none 64-t.o

64-t.o: 64-t.s
	ca65 -o 64-t.o 64-t.s -l 64-t.listing.txt

clean:
	rm *.o 64-t.prg 64-t.listing.txt