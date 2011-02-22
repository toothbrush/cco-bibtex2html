all: bib2html clean

bib2html: force_look
	ghc -O --make -o bib2html Main.hs -i..

clean:
	-rm *.hi *.o
	-rm bib2html

force_look:
	true
