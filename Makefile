all: parse-bib bib2html pp-html

parse-bib: force_look
	cd parse-bib; $(MAKE) $(MFLAGS)

bib2html: force_look
	cd bib2html; $(MAKE) $(MFLAGS)

pp-html: force_look
	cd pp-html; $(MAKE) $(MFLAGS)

documentation: force_look
	cabal haddock

force_look:
	true
