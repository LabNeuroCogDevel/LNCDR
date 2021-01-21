.PHONY: all test localinstall
all: test

localinstall:
	Rscript -e "devtools::document(); remotes::install_local('./', force=T)"
test: localinstall
	cd tests/ && Rscript "testthat.R"
