FROM rocker/tidyverse
# longer running depends so they're cached
# options(repos=structure(c(CRAN="http://lib.stat.cmu.edu/R/CRAN")))
RUN R -e "\
install.packages(c('itsadug','XML','oro.nifti', 'qualtRics','RPostgreSQL', 'gridExtra', 'pscyh','BiocManager'));\
BiocManager::install('BiocParallel');\
remotes::install_github('Jfortin1/neuroCombatData');\
remotes::install_github('Jfortin1/neuroCombat_Rpackage');"
RUN bash -c "cpan install URI::Encode<<<yes" # used for testing db uri; adds a minute to build
COPY . /lncdr
RUN R -e "remotes::install_local('lncdr', upgrade=F)"
