PACKAGES=batteries,str
SOURCES=day01.ml day02.ml day03.ml day04.ml day05.ml day06.ml

all: byte
	@echo "Merry Christmas!"

byte: $(SOURCES:.ml=.byte)


native: $(SOURCES:.ml=.native)

clean:
	rm -f *.byte *.native
	rm -rf _build

%.byte : %.ml
	ocamlbuild -use-ocamlfind $@ -pkgs $(PACKAGES)

%.native: %.ml
	ocamlbuild -cflags -unsafe -use-ocamlfind $@ -pkgs $(PACKAGES)
