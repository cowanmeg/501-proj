latex=pdflatex -synctex=1 -halt-on-error --interaction=nonstopmode
bibtex=bibtex
texfile=report

.PHONY: all clean

all: $(texfile).pdf

clean:
	rm -f $(texfile).pdf $(texfile).blg $(texfile).log $(texfile).aux $(texfile).bbl $(texfile).synctex.gz

%.pdf: %.tex %.bib
	$(latex) $(basename $@)
	$(bibtex) $(basename $@)
	$(latex) $(basename $@)
	$(latex) $(basename $@)
