all: pp-html clean

pp-html: force_look
	ghc -O --make -o pp-html Main.hs -i..

clean:
	-rm *.hi *.o

force_look:
	true
