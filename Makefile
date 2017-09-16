all:
	pdflatex single.tex 
	bibtex single
	pdflatex single.tex 
	bibtex single
	pdflatex single.tex 
