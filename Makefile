all:
	pdflatex proposal.tex 
	bibtex proposal
	pdflatex proposal.tex 
	bibtex proposal
	pdflatex proposal.tex 

prospectus:
	pdflatex prospectus.tex 
	bibtex prospectus
	pdflatex prospectus.tex 
	bibtex prospectus
	pdflatex prospectus.tex 
