# Title     : Modelling mit small subset for EO College
# Created by: Konstantin Schellenberg
# Created on: 04.01.2022

library(tidyverse)          # covenient data handling "verbs"
library(raster)             # raster reading (binding to gdal)
library(sf)                 # spatial vector handling, conplying with tidyvese
library(mlr3)               # mother package of the machine learning framework
library(mlr3spatiotempcv)   # spatiotemporal resampling methods
library(mlr3learners)
library(exactextractr)      # fast pixel extraction (bypassing `raster`'s slow `extract` function)
library("mlr3viz")  # mlr³ specific visualisation
library(kknn)

# load helper functions
source("./R/_helpers.R")

# main path (   ADJUST TO YOUR MACHINE  )
path = "D:/Geodaten/GEO402/05_Tutorial"

# ---------------------------------------
# load dataset of the first year (2015)

ds = list.files(path, pattern = "DataStack.*2017\\.img$", full.names = TRUE)
layername_in = list.files(path, pattern = "LayerNames.*2017", full.names = TRUE)
layername = read_csv(layername_in)
print(layername)

# read in raster
ras = brick(ds)

# assign layername (for convenience)
names(ras) = layername$layer

# show data
#plot(ras[[1:4]])
#plot(ras[[23:26]])
#rasterVis::levelplot(ras[[10]])
#rasterVis::levelplot(ras[[25]])

# ---------------------------------------
# loading samples (previously created via Google Earth informed visual mapping in QGIS)

lc = read_sf(file.path(path, "Features.gpkg"), layer = "samples")

# creating training/test set
lc_dfs = exact_extract(ras, lc,
                        include_cols = c("slangbos", "class"),
                        include_xy = TRUE)
# bind data frames
lc_df = bind_rows(lc_dfs)
names(lc_df)
dim(lc_df)

# filter only pixels that overlay at least 50% with the polygons
lc_df_ovly = dplyr::filter(lc_df, coverage_fraction >= 0.5) %>%
    dplyr::select(-coverage_fraction)

# Date

# registering x and y coordinates as spatial points
lc_sf = sf::st_as_sf(lc_df_ovly, coords = c("x", "y"))

# the target variable (here the binary class attribution "slangbos" needs to be converted to the factor class)
lc_sf = mutate_at(lc_sf, vars(slangbos, class), as.factor)
# note the namespaces `::` as not need when the packages is loaded, yet is given here for clearity

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

lnr_RF = lrn("classif.ranger", predict_type = "prob")
lnr_KNN = lrn("classif.kknn", predict_type = "prob")

# set hyperparameters
# todo: where are the hyperparameters from?
mtry = sqrt(task_slangbos_sp$ncol) %>% ceiling()
lnr_RF$param_set$values = list(importance = "impurity", num.trees = 300, mtry = mtry, min.node.size = 5)
lnr_KNN$param_set$values = list(k = 9)

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

# -----------------------------------
# TRAIN MODELS
# finally train model on all avaiable data

model_RF = lnr_RF$train(task_slangbos_sp)
model_KNN = lnr_KNN$train(task_slangbos_sp)

# ---------------------------------------
# prediction

dt_ras = as.data.table.raster(ras, xy = TRUE)

predict_and_save = function(task, model, outname){
  # predict on the entire scene
  pred = model$predict_newdata(task = task_slangbos_sp, newdata = dt_ras) %>%
      as.data.table()

  # wrapper for writing out
  pred.geo = georeferencing(prediction = pred, pre_prediction = dt_ras, crs = 32735)
  writeRaster(pred.geo, filename = file.path(path, sprintf("%s.tif", outname)), overwrite = TRUE)
}

predict_and_save(model = model_RF, task = task_slangbos_sp, outname = "RF")
predict_and_save(model = model_KNN, task = task_slangbos_sp, outname = "KNN")


# -----------------------------------
# Automatise for all years (operational modelling)

performance_estimation = function (){

}

performance_estimation()
predict_and_save()

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
