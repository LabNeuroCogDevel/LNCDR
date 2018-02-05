.PHONY: all test localinstall
all: test

localinstall:
	Rscript -e "devtools::document(); devtools::install('./')"
test: localinstall
	cd tests/ && Rscript "testthat.R"
