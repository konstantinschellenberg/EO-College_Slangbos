#' Introduction to mlr³ as framework for modelling, assessing machine learning algorithms in the context of Earth Observation
#' Created: Jan 5 2022
#' University of Jena, Institute for Geography, Department for Earth Observation
#' Konstantin Schellenberg

# install main packages
main_packages = c("tidyverse", "raster", "sf")
install.packages(main_packages)
install.packages("sf")

mlr_packages = c("mlr3", "mlr3spatiotempcv", "mlr3learners")
sapply(mlr_packages, install.packages)
install.packages("mlr3spatiotempcv")
sapply(mlr_packages, require, character=TRUE)


