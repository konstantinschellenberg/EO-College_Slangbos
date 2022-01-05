#' Introduction to mlr³ as framework for modelling, assessing machine learning algorithms in the context of Earth Observation
#' Created: Jan 5 2022
#' University of Jena, Institute for Geography, Department for Earth Observation
#' Konstantin Schellenberg

# install main packages
main_packages = c("tidyverse", "raster", "sf")
mlr_packages = c("mlr3", "mlr3spatiotempcv", "mlr3learners")

# (   UNCOMMENT TO INSTALL PACKAGES   )
# install.packages(main_packages)
# install.packages(mlr_packages)

# loading packages
all_loaded = sapply(c(main_packages, mlr_packages), require, character=TRUE)

# are all packages successfully installed and loaded?
all(all_loaded)
# -> must return TRUE

# -----------------------------------
# Quick mlr³ example with spatial data

# get data
data(ecuador)

tsk("ecuador")

