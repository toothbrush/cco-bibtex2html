DIRS = parse-bib-src bib2html-src pp-html-src

all: parse-bib bib2html pp-html clean

parse-bib: force_look
	cd parse-bib-src; $(MAKE) $(MFLAGS)

bib2html: force_look
	cd bib2html-src; $(MAKE) $(MFLAGS)

pp-html: force_look
	cd pp-html-src; $(MAKE) $(MFLAGS)

clean:
	-for d in $(DIRS); do (cd $$d; $(MAKE) clean ); done
	-rm common/*.{hi,o}
	-rm pp-html bib2html parse-bib

documentation: force_look
	cabal haddock

install: 
	cp parse-bib-src/parse-bib .
	cp bib2html-src/bib2html .
	cp pp-html-src/pp-html .

.PHONY: install

force_look:
	true
