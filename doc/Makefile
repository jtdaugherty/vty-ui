
manual.pdf: *.tex
	# Run it twice so the TOC gets generated properly
	pdflatex manual.tex
	pdflatex manual.tex

clean:
	rm -f *~ *.dvi *.pdf *.log *.aux *.toc *.out
