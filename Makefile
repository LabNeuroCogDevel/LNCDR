.PHONY: all test localinstall
all: test

localinstall:
	Rscript -e "devtools::document(); setwd('..'); devtools::install('LNCDR')"
test: localinstall
	cd tests/ && Rscript "testthat.R"
