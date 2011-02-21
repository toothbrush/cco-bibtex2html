default: pp-html clean

code: Main.hs

pp-html: Main.hs
	ghc -O --make -o pp-html Main.hs -i..

clean:
	rm *.hi
	rm *.o
