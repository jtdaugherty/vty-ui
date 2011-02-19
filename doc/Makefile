
manual.pdf: manual.tex
	# Run it twice so the TOC gets generated properly
	pdflatex manual.tex
	pdflatex manual.tex

clean:
	rm -f manual.dvi *~ manual.pdf manual.log manual.aux
