
manual.pdf: *.tex ch[1234]/*.tex
	# Run it twice so the TOC gets generated properly
	pdflatex -shell-escape manual.tex
	pdflatex -shell-escape manual.tex

clean:
	rm -f *~ *.dvi *.pdf *.log *.aux *.toc *.out
	rm -f ch[1234]/*.aux ch[1234]/*~
