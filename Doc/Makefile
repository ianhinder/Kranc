
LATEX = latex --interaction=nonstopmode 
PDFLATEX = pdflatex --interaction=nonstopmode
#-file-line-error-style

default: KrancDoc.pdf

help:	
	@echo "Type make <file>.ps to build from latex source. Can also make <file>.pdf and <file>.html"

clean:	
	rm -rf *.{dvi,ps,aux,log,pdf}

%.dvi:	%.tex
	$(LATEX) $<

%.ps:	%.dvi
	dvips -o $@ $<

%.html:	%.tex
	htlatex $<

%.pdf:	%.tex
	$(PDFLATEX) $<
