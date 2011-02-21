default: bib2html clean

code: Main.hs

bib2html: Main.hs
	ghc -O --make -o bib2html Main.hs -i..

clean:
	rm *.hi
	rm *.o
