default: parse-bib clean

code: ParseBib.hs

parse-bib: ParseBib.hs
	ghc -O --make -o parse-bib ParseBib.hs -i..

clean:
	rm *.hi
	rm *.o
