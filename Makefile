.PHONY: clean
.DEFAULT_GOAL: 64-t.prg

objects = 64-t.o

64-t.prg: $(objects) bin
	ld65 -o bin/64-t.prg -C 64-t.cfg -m obj/64-t.map.txt $(addprefix obj/, $(objects))

$(objects): %.o: %.s obj
	ca65 -o obj/$@ -t c64 -l obj/$*.listing.txt $<

bin:
	mkdir bin

obj:
	mkdir obj

clean:
	-rm -rf bin
	-rm -rf obj