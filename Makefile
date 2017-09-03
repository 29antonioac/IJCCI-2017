all: Prediction.pdf

sections := $(wildcard sections/*.tex)

Prediction.pdf: $(sections) Prediction.tex prediction.bib
	pdflatex Prediction.tex && bibtex Prediction.aux && pdflatex Prediction.tex && pdflatex Prediction.tex

Prediction.tex: Prediction.Rnw $(Rnw)
	R -e 'library(knitr);knit("Prediction.Rnw")'

install:
	R -e 'install.packages(c("knitr","ggplot2","reshape2", "xtable","stringi"), repos="http://cran.us.r-project.org")'

clean:
	rm *.aux *.pdf *.tex
