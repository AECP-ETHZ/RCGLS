cd RCGLS
rm RCGLS*.tar.gz
rm NAMESPACE

RScript -e 'library(devtools); devtools::document(); devtools::document()'
R CMD build .
R CMD check --as-cran RCGLS*.tar.gz
