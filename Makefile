all:
	Rscript -e "devtools::document(); setwd('..'); devtools::install('LNCDR')"
test:
	cd tests/ && Rscript "testthat.R"
