all:
	Rscript -e "devtools::document(); setwd('..'); devtools::install('LNCDR')"
