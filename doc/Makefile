
vty-ui-users-manual.pdf: *.tex ch?/*.tex
	# Run it twice so the TOC gets generated properly
	pdflatex -shell-escape vty-ui-users-manual.tex
	pdflatex -shell-escape vty-ui-users-manual.tex

clean:
	rm -f *~ *.dvi *.pdf *.log *.aux *.toc *.out
	rm -f ch?/*.aux ch?/*~
