.PHONY: clean
.DEFAULT_GOAL: all

all: 64-t.prg 64-tng.prg

64-t.prg: 64-t.o bin
	ld65 -o bin/64-t.prg -C src/64-t.cfg -m obj/64-t.map.txt $(addprefix obj/, 64-t.o)

64-tng.prg: 64-tng.o newmodem.o bin
	ld65 -o bin/64-tng.prg -C src/64-tng.cfg -m obj/64-tng.map.txt $(addprefix obj/, 64-tng.o newmodem.o)

64-t.o 64-tng.o newmodem.o: %.o: src/%.s obj
	ca65 -o obj/$@ -t c64 -l obj/$*.listing.txt $<

bin:
	mkdir bin

obj:
	mkdir obj

clean:
	-rm -rf bin
	-rm -rf obj