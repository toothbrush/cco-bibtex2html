all: parse-bib clean

parse-bib: force_look
	ghc -O --make -o parse-bib Main.hs -i..

clean:
	-rm *.hi *.o

force_look:
	true
