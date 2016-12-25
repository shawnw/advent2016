PACKAGES=batteries,str
SOURCES=day01.ml day02.ml day03.ml day04.ml day05.ml day06.ml day07.ml \
	day08.ml day09.ml day10.ml day11.ml day11v2.ml \
	day12.ml

all: byte
	@echo "Merry Christmas!"

byte: $(SOURCES:.ml=.byte)


native: $(SOURCES:.ml=.native)

clean:
	rm -f *.byte *.native
	rm -rf _build

%.byte : %.ml
	ocamlbuild -use-ocamlfind -cflags -safe-string $@ -pkgs $(PACKAGES)

%.native: %.ml
	ocamlbuild -cflags -unsafe,-safe-string -use-ocamlfind $@ -pkgs $(PACKAGES)
