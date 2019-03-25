all:
	pdflatex thesis.tex 
	bibtex thesis
	pdflatex thesis.tex 
	bibtex thesis
	pdflatex thesis.tex 

fast:
	pdflatex thesis.tex 

fast2:
	pdflatex thesis.tex 
	pdflatex thesis.tex 

sym:
	pdflatex symb.tex 
	bibtex symb
	pdflatex symb.tex 
	bibtex symb
	pdflatex symb.tex 

proposal:
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

pldi:
	pdflatex pldi19.tex 
	bibtex pldi19
	pdflatex pldi19.tex 
	bibtex pldi19
	pdflatex pldi19.tex 

fast-pldi:
	pdflatex pldi19.tex 

clean:
	rm -f *.aux *.bbl *.blg *.log *.out *.toc *.lof *.lod *.loe *-blx.bib *.run.xml
