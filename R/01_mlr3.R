#' Introduction to mlr³ as framework for modelling, assessing machine learning algorithms in the context of Earth Observation
#' Created: Jan 5 2022
#' University of Jena, Institute for Geography, Department for Earth Observation
#' Konstantin Schellenberg

# install main packages
main_packages = c("tidyverse", "raster", "sf")
mlr_packages = c("mlr3", "mlr3spatiotempcv", "mlr3learners", "mlr3filters", "mlr3viz")

# (   UNCOMMENT TO INSTALL PACKAGES   )
# install.packages(main_packages)
# install.packages(mlr_packages)

# loading packages
all_loaded = sapply(c(main_packages, mlr_packages), require, character=TRUE, quietly=TRUE)

# are all packages successfully installed and loaded?
all(all_loaded)
# -> must return TRUE

theme_set(theme_minimal())

# -----------------------------------
# TASKS
# Quick mlr³ example with spatial data

# possible predefined data sets
as.data.table(mlr_tasks)


# -----------------------------------
# Learners

as.data.table(mlr_resamplings)

# -----------------------------------
# Resampling

as.data.table(mlr_resamplings)

# -----------------------------------
# Train & Predict



# get dataset (Muenchow et al. 2012)
data(ecuador)

# registering x and y coordinates as spatial points
sf_ecuador = sf::st_as_sf(ecuador, coords = c("x", "y"))
# note the namespaces `::` as not need when the packages is loaded, yet is given here for clearity

# quick visualisation
ggplot(sf_ecuador, aes(color = slides)) +
    geom_sf() +
    labs(x = "x", y = "y", title = "Landslide occurrence", color = "")

# create a classification task instance, specifying the target variable (Landslide occurrence)
#task = TaskClassif$new(id = "ecuador", backend = ecuador, target = "slides")
# `TaskClassif` for all non-spatial analyses

# `TaskClassifST` is the SpatioTemporal version of classification tasks
# Easy: Backend is `sf` object with point coordinates
# Otherwise: for data.frame/data.table: extra_args = list(coordinate_names = c("x", "y")
task = TaskClassifST$new(id = "ecuador", backend = sf_ecuador, target = "slides")

# inspect task
print(task)


#' More information on tasks
#'
#'
#'

# ---------------------------------------
# specifying a classification algorithm `learner`
learner = lrn("")



