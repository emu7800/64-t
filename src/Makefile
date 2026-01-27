.PHONY: all clean
.DEFAULT_GOAL: all

all: 64-t.prg 64-tng.prg

64-t.prg: 64-t.o
	ld65 -o 64-t.prg -C 64-t.cfg -m 64-t.map.txt 64-t.o

64-tng.prg: 64-tng.o rs232_userport.o finescroll.o
	ld65 -o 64-tng.prg -C 64-tng.cfg -m 64-tng.map.txt 64-tng.o rs232_userport.o finescroll.o

%.o: %.s
	ca65 -o $@ -t c64 -l $*.listing.txt $<

clean:
	-rm -f *.o *.prg *.listing.txt *.map.txt
