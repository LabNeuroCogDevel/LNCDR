FROM rocker/tidyverse
# longer running depends so they're cached
RUN R -e "\
install.packages(c('itsadug','XML','oro.nifti', 'qualtRics','RPostgreSQL', 'gridExtra', 'pscyh','BiocManager'));\
BiocManager::install('BiocParallel');\
remotes::install_github('Jfortin1/ComBatHarmonization/R/neuroCombat');\
"
COPY . /lncdr
RUN R -e "remotes::install_local('lncdr')"
