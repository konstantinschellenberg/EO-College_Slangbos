#' Title     : Dry Ecosystems - Slangbos encroachment mapping in South Africa
#' Subtitle  : Introduction to mlr³ as framework for modelling, assessing machine learning algorithms in the context of Earth Observation
#' Project   : EO College
#' Created by: Konstantin Schellenberg
#' University of Jena, Institute for Geography, Department for Earth Observation
#' Created on: 04.01.2022
#' Created for: EO-College - Dry Ecosystems
#' Scrip No. 1

#' Scope of the script
#' __________________
#' This script introduces data pre-processing techniques to wragle multi-dimensional (lat, lon, time) remote sensing data
#' to the mlr³ machine learning (ML) framework. Further, a simple (KNN) and an advanced (RandomForest) ML algorithm is
#' used to model the spatial abundance of slangbos on the basis of field reference data. Unbiased assessment of spatial
#' model performance is undertaken using spatial cross-validation (chunking the dataset spatially to training and testing).
#' The RandomForest feature importance is used to determine the relative significance of a specific variable when accounting
#' for the contribution of all other variables in the mode.
#'

# -----------------------------------
# R VERSION USED FOR TUTORIAL:
# R 4.1.2

# YOUR R-VERSION
cat(version$version.string)
# (using other version than 4.1.2 can be unstable and may result in unexpected behaviour)

# -----------------------------------
# install main packages
main_packages = c("tidyverse", "terra", "sf", "data.table", "ranger", 
                  "exactextractr", "kknn")
mlr_packages = c("mlr3", "mlr3learners",
                 "mlr3filters", "mlr3viz")

# ancillary packages
# install.packages(c("patchwork", "ggtext", "ggsci"))

# (   UNCOMMENT TO INSTALL PACKAGES   )
# install.packages(main_packages)
# install.packages(mlr_packages)

# install mlr3 spatial backend lib from github (due to errors in CRAN version 2.0.1)
remotes::install_github("mlr-org/mlr3spatiotempcv")

# loading packages
all_loaded = sapply(c(main_packages, mlr_packages), require, character=TRUE, quietly=TRUE)

# are all packages successfully installed and loaded?
all(all_loaded)
# -> must return TRUE, then all required packages are loaded in the session

# -----------------------------------
# alternative loading packages
library(tidyverse)          # covenient data handling "verbs"
# library(raster)           # raster reading (binding to gdal)
library(terra)              # superceded `raster`. raster reading (binding to gdal)
library(sf)                 # spatial vector handling, conplying with tidyvese
library(mlr3)               # mother package of the machine learning framework
library(mlr3spatiotempcv)   # spatiotemporal resampling methods
library(mlr3learners)       # additional classification and regression algorithms for mlr³
library(mlr3viz)            # mlr³ specific visualisation
library(mlr3filters)        # layer importance estimation
library(exactextractr)      # fast pixel extraction (bypassing `raster`'s slow `extract` function)

library(kknn)               # Weighted k-Nearest Neighbor Classifier
# library(ranger)           # not needing to load, is loaded with mlr3learners

# setting a clean ggplot theme
theme_set(theme_minimal())

# main path (can be changed if the data is located not in the R project folder system)
path = "./"

# ---------------------------------------
# PATHS

# basic paths needed for the tutorial
path_data = file.path(path, "data")
path_results = file.path(path, "data", "results")
path_datacube = file.path(path, "data", "Cube")
# the paths are also included in _helpers.R, so that their are loaded with this file is included:
source("./R/_helpers.R")

# bulk create paths, otherwise warning
walk(list(path_data, path_datacube, path_results), ~ dir.create(.x, recursive = TRUE))

# -----------------------------------
# MODELLING FIRST YEAR (2015) ONLY

# loading dataset
dataset.in = list.files(path_datacube, pattern = "DataStack.*2015\\.img$", full.names = TRUE)
layername.in = list.files(path_datacube, pattern = "^Lay.*2015", full.names = TRUE)
layername = read_csv(layername.in)
print(layername)

# read in raster
ras = rast(dataset.in)

# assign layername to the raster bands (for convenience)
names(ras) = layername$layer

# show data
plot(ras[[1]], main = layername$layer[1])  # SAVI
plot(ras[[23]], main = layername$layer[23]) # VH

# ---------------------------------------
# loading samples: Training/test set for classification
# (previously created via Google Earth informed visual mapping in QGIS)
lc = read_sf(file.path(path_data, "Features.gpkg"), layer = "samples")
# spatial feature `sf` object (inherent geospatial information: x, y)

# remarks on crs: spatial vector and raster data must be projected in the same coordinate system!
crs(ras) = sprintf("epsg:%s", st_crs(lc)$epsg)
crs(ras, describe = TRUE)$code
st_crs(lc)$epsg


# creating training/test set.
# Indexes and extracts each pixel overlapping the polygons in time and layer
# `include_xy`: carry spatial information through the extraction process
lc_dfs = exactextractr::exact_extract(ras, lc,
                       include_cols = c("slangbos", "class"),
                       include_xy = TRUE)

# created a list of data.frames for each polygon
length(lc_dfs) == nrow(lc)  # equals the rows (sample polygons) previous imported
# merge this list together
lc_df = bind_rows(lc_dfs)

# one line
lc_df[1,]
# all variables
names(lc_df)
# dimersionality of the training/test set
dim(lc_df)

# Interpretation:
# rows: pixel
# cols: layertype (SAVI, VH), acquisiton date

# filter only pixels that overlay at least 50% with the polygons
lc_df_filtered = dplyr::filter(lc_df, coverage_fraction >= 0.5) %>%
    dplyr::select(-coverage_fraction)
# note the namespaces `::` as not need when the packages is loaded, yet is given here for clearity

# registering x and y coordinates as spatial points (again)
lc_sf = sf::st_as_sf(lc_df_filtered, coords = c("x", "y"))

# For mlr³, the target variable needs to be converted to the factor class
# 1. Binary classification: "slangbos"
# 2. Multiclass classification: "class"
lc_sf = mutate_at(lc_sf, vars(slangbos, class), as.factor)

# Quick visualisation
ggplot(lc_sf) + geom_sf(aes(color = slangbos))
ggplot(lc_sf) + geom_bar(aes(as.factor(class), fill = as.factor(slangbos))) +
    labs(x = "Classes", y = "Count", fill = "Slangbos (yes/no)")

# choose `twoclass` / binary or `multiclass` problem:
# Binary:
lc_binary = dplyr::select(lc_sf, -class)

# Multiclass:
lc_multiclass = dplyr::select(lc_sf, -slangbos)

# --------------------------------------- 1 -----------------------------------
# TASK = type of aspired analysis, contain the backend data

# `TaskClassifST` is the SpatioTemporal version of classification tasks
# Easy: Backend is `sf` object with point coordinates
# Otherwise: for data.frame/data.table: extra_args = list(coordinate_names = c("x", "y"))
task_slangbos_sp = TaskClassifST$new(id = "Slangbos2015", backend = lc_binary, target = "slangbos")
task_slangbos_sp = TaskClassifST$new(id = "Slangbos2015", backend = lc_multiclass, target = "class")

# --------------------------------------- 2 -----------------------------------
# LEARNER = classification algorithm
# Testing two different learners:
# 1. Random Forest RF (package `ranger`), importance attribute only important for assessing feature importance (step 7)
lnr_RF = lrn("classif.ranger", predict_type = "prob", importance = "permutation")

# 2. K-nearst neighbor KNN (package `kknn`)
lnr_KNN = lrn("classif.kknn", predict_type = "prob")

# Integral characteristic of learners: hyperparameters (HP)
# Decision:
#   (1) Informed setting of HP,
#   (2) tuning of HP
# Random Forest robust in terms of HP setting (source), (1) feasable.
# Most other algorithms, yet, require HP optimisation. See https://mlr3book.mlr-org.com/optimization.html

# --------------------------------------- 3 -----------------------------------
# 1. Random Forest

# mtry: size of split options for each tree
# fixed to [sqrt(p)], where p is the number of varibles in the model. Commonly used for classification tasks (Hastie et al., 2009)
mtry = sqrt(task_slangbos_sp$ncol) %>% ceiling()  # 6

# number of trees: number of trees of created (RF is biased only to very little num of trees, 300 trade-off between computation effort and unbiasedness)
num.trees = 300

# define HP for the learner
lnr_RF$param_set$values = list(importance = "impurity", num.trees = num.trees, mtry = mtry)
# all others are kept default

# 2. KNN
# k: Number of considered neighbors, arbitrary. Needs to be optimised (tuned) when used operationally
lnr_KNN$param_set$values = list(k = 9)

# --------------------------------------- 4 -----------------------------------
# TRAIN = fit a learner to all the available data, creates a MODEL

# finally train model on all avaiable data
model_RF = lnr_RF$train(task_slangbos_sp)
model_KNN = lnr_KNN$train(task_slangbos_sp)

# --------------------------------------- 5 -----------------------------------
# PREDICT = use a MODEL to predict on new data (i.e. a entire EO scene)

predict_and_save = function(task, model, newdata, outfile, crs=32735, return=FALSE){
    #' task: mlr3 Task with data in backend
    #' model: the fitted `learner` on the task.
    #' newdata: raster data of the entire scene in data.table format (RS scene and coords as features). coords must
    #'      be named `x` and `y`.
    #' outfile: Prediction output file.
    #' crs: coordinate reference system in EPSG
    #' return: whether or not saving the prediction as R variable

    # predict on the entire scene
    pred = model$predict_newdata(task = task, newdata = newdata) %>% as.data.table()

    # wrapper for writing out
    coords = newdata[, .(x, y)]
    pred.geo = georeferencing(prediction = pred, vec_coords = coords, crs = crs)

    # change order
    pred.geo.swap = c(pred.geo[[2]], pred.geo[[3]], pred.geo[[1]])

    terra::writeRaster(pred.geo.swap, filename = outfile, filetype = "GTiff", overwrite = TRUE, datatype = "FLS4S")
    if (isTRUE(return)) return(pred.geo)
}

# transform raster data to data.table, loads into memory.
# CAUTION: very large rasters can hit the RAM limit, consider swap files.
dt_ras = as.data.table(as.data.frame(ras, xy = TRUE))

outfileRF = file.path(path_results, sprintf("%s.tif", "TestRF"))
outfileKNN = file.path(path_results, sprintf("%s.tif", "TestKNN"))

# RF
predictionRF = predict_and_save(model = model_RF, task = task_slangbos_sp,
                                newdata = dt_ras, outfile = outfileRF, return = TRUE)

# KNN
predictionKNN = predict_and_save(model = model_KNN, task = task_slangbos_sp,
                                 newdata = dt_ras, outfile = outfileKNN, return = TRUE)

# -----------------------------------
# VISUALISATION

# inspect resulting layers
# 1. binary/multiclass response variable
# > 2. Probability of class membership for each class (here: binary TRUE/FALSE)
plot(predictionRF)

 # binary response
par(mfrow = c(1,2))   # show two plot side-on-side
plot(predictionRF[[1]], main = "Random Forest classifier")
plot(predictionKNN[[1]], main = "KNN classifier (k = 9)")
# values: 1 = FALSE, 2 = TRUE

# probability of class membership.
plot(predictionRF[[3]], main = "Random Forest classifier")
plot(predictionKNN[[3]], main = "KNN classifier (k = 9)")

# --------------------------------------- 6 -----------------------------------
# VALIDATION = RESAMPLING internal training/test set patitition for assessing accuracy of the algorithm
# In case of spatial data, the training/test sets need to be chosen in an spatially unbiased fashion to avoid spatial autocorrelation

# spatial resampling
# 1. RF
# SPATIAL CROSS-VALIDATION
resampling_RF_SpCV = rsmp("repeated_spcv_coords", folds = 10L, repeats = 10L)
resampling_RF_SpCV$instantiate(task = task_slangbos_sp)
autoplot(resampling_RF_SpCV, task_slangbos_sp)

# shows the test set that is holded out for independent model fit on the traing set only
resampling_RF_SpCV$train_set(1)
resampling_RF_SpCV$test_set(1)

# CROSS-VALIDATION
resampling_RF_CV = rsmp("repeated_cv", folds = 10, repeats = 10L)
resampling_RF_CV$instantiate(task = task_slangbos_sp)
autoplot(resampling_RF_CV, task_slangbos_sp)

# +++++++++++
# 2. KNN
# SPATIAL CROSSVALIDATION
resampling_KNN_SpCV = rsmp("repeated_spcv_coords", folds = 10L, repeats = 10L)
resampling_KNN_SpCV$instantiate(task = task_slangbos_sp)

# +++++++++++
# validation on spatial data splits in training and test sets
# CAUTION: CAN TAKE LONGER! Adjust/reduce resampling parameters (folds, repeats according to your computer's ability)
rr_RF_SpCV = mlr3::resample(task_slangbos_sp, lnr_RF, resampling_RF_SpCV, store_models = TRUE)
rr_RF_CV = mlr3::resample(task_slangbos_sp, lnr_RF, resampling_RF_CV, store_models = TRUE)
rr_KNN_SpCV = mlr3::resample(task_slangbos_sp, lnr_RF, resampling_KNN_SpCV, store_models = TRUE)

# inspection of results (accuracy = 1-error), more measures like "area under ROC curve" or "classification error" available
rr_RF_CV$aggregate(msr("classif.acc"))   # biased estimate, overestimation of accuracy

rr_RF_SpCV$aggregate(msr("classif.acc")) # (fairly) unbiased estimate (Brenning et al., 2012)
rr_RF_SpCV$score()[1:10]   # show result of the first 10 single repetition

rr_KNN_SpCV$aggregate(msr("classif.acc"))   # biased estimate, overestimation of accuracy


# --------------------------------------- 7 -----------------------------------
# Feature Importance = Which layers have been imported for model fit?

# create filter instance (permutation importance is a metric inherent to RF)
filter = flt("importance", learner = lnr_RF)

filter$calculate(task_slangbos_sp)
filter_results = as.data.table(filter)
autoplot(filter) + theme(axis.text.x = element_text(angle = 90, size = 10))

# representation on the time dimension (add date and layer description)
filter_results_date = filter_results %>%
    dplyr::mutate(date = as.Date(str_extract(feature, "\\d{4}\\.\\d{2}.\\d{2}"), format = "%Y.%m.%d")) %>%
    dplyr::mutate(layer = str_extract(feature, "^[a-z]{2,4}"))

ggplot(filter_results_date, aes(date, score, fill = layer)) +
    geom_bar(stat = "identity", width = 5)

# interpret!
# note that the importance is temporally correlated!

# -----------------------------------
# --> next script 02_automatise to training/prediction on all years
