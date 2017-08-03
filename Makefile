all: Prediction.pdf

sections := $(wildcard sections/*.tex)

Prediction.pdf: $(sections) Prediction.tex prediction.bib
	pdflatex Prediction.tex && bibtex Prediction.aux && pdflatex Prediction.tex && pdflatex Prediction.tex
