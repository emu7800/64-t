.PHONY: all clean
.DEFAULT_GOAL: all

all: bin obj
	make -C src
	cp src/*.prg bin/
	cp src/*.o src/*.txt obj/

bin:
	mkdir bin/

obj: src
	mkdir obj/

clean:
	-rm -rf bin/ obj/
	make clean -C src
