PACKAGES=batteries,str
SOURCES=day01.ml


all: $(SOURCES:.ml=.byte)
	@echo "Merry Christmas!"

native: $(SOURCES:.ml=.native)

clean:
	rm -f *.byte *.native
	rm -rf _build

%.byte : %.ml
	ocamlbuild -use-ocamlfind $@ -pkgs $(PACKAGES)

%.native: %.ml
	ocamlbuild -use-ocamlfind $@ -pkgs $(PACKAGES)
