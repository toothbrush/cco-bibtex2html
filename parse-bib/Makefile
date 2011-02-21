default: parse-bib clean

code: Main.hs

parse-bib: Main.hs
	ghc -O --make -o parse-bib Main.hs -i..

clean:
	rm *.hi
	rm *.o
