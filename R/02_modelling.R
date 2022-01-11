#' Title     : Modelling mit small subset for EO College
#' Created by: Konstantin Schellenberg
#' Created on: 04.01.2022
#' Created for: EO-College - Dry Ecosystems

library(tidyverse)          # covenient data handling "verbs"
library(raster)             # raster reading (binding to gdal)
library(sf)                 # spatial vector handling, conplying with tidyvese
library(mlr3)               # mother package of the machine learning framework
library(mlr3spatiotempcv)   # spatiotemporal resampling methods
library(mlr3learners)       # additional classification and regression algorithms for mlr³
library(exactextractr)      # fast pixel extraction (bypassing `raster`'s slow `extract` function)
# library("mlr3viz")        # mlr³ specific visualisation
library(kknn)               # Weighted k-Nearest Neighbor Classifier

# load helper functions
source("./R/_helpers.R")

# main path (   ADJUST TO YOUR MACHINE  )
path = "/home/c3urma/Projects/EO-College_Slangbos/"

# ---------------------------------------
# PATHS

# basic paths needed for the tutorial
path_data = file.path(path, "data")
path_results = file.path(path, "results")
path_datacube = file.path(path, "data", "Cube")

# bulk create paths
walk(list(path_data, path_datacube, path_results), ~ dir.create(.x, recursive = TRUE))
# -----------------------------------
# Load data

# load dataset of the first year (2015)
ds = list.files(path_datacube, pattern = "DataStack.*2015\\.img$", full.names = TRUE)
layername_in = list.files(path_datacube, pattern = "^Lay.*2015", full.names = TRUE)
layername = read_csv(layername_in)
print(layername)

# read in raster
ras = brick(ds)

# assign layername to the raster bands (for convenience)
names(ras) = layername$layer

# show data
plot(ras[[1]])
plot(ras[[23:26]])
# rasterVis::levelplot(ras[[10]])
# rasterVis::levelplot(ras[[25]])

# ---------------------------------------
# loading samples: Training/test set for classification
# (previously created via Google Earth informed visual mapping in QGIS)
lc = read_sf(file.path(path_data, "Features.gpkg"), layer = "samples")
# spatial feature `sf` object (inherent geospatial information: x, y)

# creating training/test set.
# Indexes and extracts each pixel overlapping the polygons in time and layer
# `include_xy`: carry spatial information through the extraction process
lc_dfs = exact_extract(ras, lc,
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

# ---------------------------------------
# TASK = type of aspired analysis, contain the backend data

# NON SPATIAL
# create a classification task instance, specifying the target variable (Landslide occurrence)
task_slangbos_nosp = TaskClassif$new(id = "Slangbos2015", backend = st_set_geometry(lc_binary, NULL), target = "slangbos")
# `TaskClassif` for all non-spatial analyses

# SPATIAL
# `TaskClassifST` is the SpatioTemporal version of classification tasks
# Easy: Backend is `sf` object with point coordinates
# Otherwise: for data.frame/data.table: extra_args = list(coordinate_names = c("x", "y"))
task_slangbos_sp = TaskClassifST$new(id = "Slangbos2015", backend = lc_binary, target = "slangbos")

# ---------------------------------------
# LEARNER = classification algorithm
# Testing two different learners:
# 1. Random Forest RF (package `ranger`)
lnr_RF = lrn("classif.ranger", predict_type = "prob")

# 2. K-nearst neighbor KNN (package `kknn`)
lnr_KNN = lrn("classif.kknn", predict_type = "prob")

# Integral characteristic of learners: hyperparameters (HP)
# Decision:
#   (1) Informed setting of HP,
#   (2) tuning of HP
# Random Forest robust in terms of HP setting (source), (1) feasable.
# Most other algorithms, yet, require HP optimisation. See https://mlr3book.mlr-org.com/optimization.html

# -----------------------------------
# HYPERPARAMETERS
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

# -----------------------------------
# TRAIN =

# finally train model on all avaiable data
model_RF = lnr_RF$train(task_slangbos_sp)
model_KNN = lnr_KNN$train(task_slangbos_sp)

# ---------------------------------------
# PREDICT

# transform raster data to data.table, loads into memory.
# CAUTION: very large rasters can hit the RAM limit, consider swap files.
dt_ras = as.data.table.raster(ras, xy = TRUE)

predict_and_save = function(task, model, newdata, outfile){
  # predict on the entire scene
  pred = model$predict_newdata(task = task, newdata = newdata) %>%
      as.data.table()

  # wrapper for writing out
  pred.geo = georeferencing(prediction = pred, pre_prediction = newdata, crs = 32735)
  writeRaster(pred.geo, filename = outfile, overwrite = TRUE)
}

outnameRF = "TestRF"
outnameKNN = "TestKNN"

predict_and_save(model = model_RF, task = task_slangbos_sp, newdata = dt_ras,
                 outfile = file.path(path_results, sprintf("%s.tif", outnameRF)))
predict_and_save(model = model_KNN, task = task_slangbos_sp, newdata = dt_ras,
                 outfile = file.path(path_results, sprintf("%s.tif", outnameRF)))


# -----------------------------------
# VALIDATION = RESAMPLING internal training/test set patitition
# In case of spatial data, the training/test sets need to be chosen in an spatially unbiased fashion to avoid spatial autocorrelation

# spatial resampling
resampling_RF1 = rsmp("repeated_spcv_coords", folds = 10, repeats = 10L)
resampling_RF2 = rsmp("repeated_cv")

resampling_RF1$instantiate(task = task_slangbos_sp)
resampling_RF2$instantiate(task = task_slangbos_sp)
autoplot(resampling_RF1, task_slangbos_sp)
autoplot(resampling_RF2, task_slangbos_sp)

# validation on spatial data splits in training and test sets
rr1 = resample(task_slangbos_sp, lnr_RF, resampling_RF1, store_models = TRUE)
rr2 = resample(task_slangbos_sp, lnr_RF, resampling_RF2, store_models = TRUE)

rr1$aggregate(msr("classif.ce"))
rr2$aggregate(msr("classif.ce"))

# ---------------------------------------
# MAPPING
dss = list.files(path, pattern = "DataStack.*img$", full.names = TRUE)


# depr
# pivoting data frame (colums "variables" of layers to rows "observations"
# crs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs"

# -----------------------------------
#task_st = tsk("cookfarm")
#resampling = rsmp("sptcv_cstf",
#  folds = 5, time_var = "Date",
#  space_var = "SOURCEID")
#resampling$instantiate(task_st)
#task_st$backend
#
#
## spatiotemporal data cube
## force long format
#lc_long = lc_sf %>% pivot_longer(cols = matches("\\d{4}"), names_to = "Variable", values_to = "EO")
## split variable into Layer (VH and SAVI) and Date
#lc_long_split = lc_long %>% mutate(Layer = str_to_upper(str_match(Variable, "^[a-z]{2,}")),
#                                   Date = str_match(Variable, "\\d{4}\\.\\d{2}\\.\\d{2}"))
## coerce Date to internal `date` format
#lc_long_split = lc_long_split %>% mutate(Date = as.character(as.Date(Date, format = "%Y.%m.%d")))
## delete "mixed" variable column
#lc_sptmp = dplyr::select(lc_long_split, -Variable)
## EO Data to separate input columns
#lc_sptmp_grouped = lc_sptmp %>%
#    filter(Layer=="SAVI") %>%
#    dplyr::select(-Layer, SAVI = EO, -class)
#
#task_slangbos_sptm = TaskClassifST$new(id = "SB_spatiotemp", backend = lc_sptmp_grouped,
#                                       target = "slangbos")
#resampling2 = rsmp("sptcv_cstf",
#  folds = 5, time_var = "Date", space_var = "SAVI")
#resampling2$instantiate(task_slangbos_sptm)
#autoplot(resampling2, task_slangbos_sptm)
#autoplot(resampling, task_st)
#
#pl = autoplot(resampling2, task_slangbos_sptm, c(1, 2, 3, 4),
#  crs = 32735, point_size = 3, axis_label_fontsize = 10,
#  plot3D = TRUE
#)
#
## Warnings can be ignored
#pl_subplot = plotly::subplot(pl)
#
#plotly::layout(pl_subplot,
#  title = "Individual Folds",
#  scene = list(
#    domain = list(x = c(0, 0.5), y = c(0.5, 1)),
#    aspectmode = "cube",
#    camera = list(eye = list(z = 2.5))
#  ),
#  scene2 = list(
#    domain = list(x = c(0.5, 1), y = c(0.5, 1)),
#    aspectmode = "cube",
#    camera = list(eye = list(z = 2.5))
#  ),
#  scene3 = list(
#    domain = list(x = c(0, 0.5), y = c(0, 0.5)),
#    aspectmode = "cube",
#    camera = list(eye = list(z = 2.5))
#  ),
#  scene4 = list(
#    domain = list(x = c(0.5, 1), y = c(0, 0.5)),
#    aspectmode = "cube",
#    camera = list(eye = list(z = 2.5))
#  )
#)
#
## -----------------------------------
## failed because SAVI and VH were not taken on the same day, thus no harmonisation possible
#
#



