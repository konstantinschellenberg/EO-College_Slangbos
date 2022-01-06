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
# the target variable (here the binary class attribution "slangbos" needs to be converted to the factor class)
samp = read_sf(file.path(path, "Features.gpkg"), layer = "samples") %>%
    mutate_at(vars(slangbos, class), as.factor)

# creating training/test set
dfs_samp = exact_extract(ras, samp,
                        include_cols = c("slangbos", "class"),
                        include_xy = TRUE)
# bind data frames
df_samp = bind_rows(dfs_samp)
names(df_samp)


# filter only pixels that cover at least 50% of the
df_samp_cov = dplyr::filter(df_samp, coverage_fraction >= 0.5) %>%
    dplyr::select(-coverage_fraction)

# Date

# visualise
sf_samp = st_as_sf(df_samp_cov, coords = c("x", "y"))
ggplot(sf_samp) + geom_sf(aes(color = class))
ggplot(sf_samp) + geom_bar(aes(as.factor(class), fill = as.factor(slangbos))) +
    labs(x = "Classes", y = "Count", fill = "Slangbos (yes/no)")

# remove class
sf_samp = dplyr::select(sf_samp, -class)

# ---------------------------------------
# modelling

# creating instance of an mlr3 classification task
task_slangbos = TaskClassifST$new(id = "Slangbos2015", backend = sf_samp,
                                target = "slangbos")

# ---------------------------------------
# create classifier
learner = lrn("classif.ranger", predict_type = "prob")

# set hyperparameters
mtry = sqrt(task_slangbos$ncol) %>% ceiling()
learner$param_set$values = list(importance = "impurity", num.trees = 300,
                                mtry = mtry, min.node.size = 5)

resampling = rsmp("repeated_spcv_coords", folds = 5L, repeats = 10L)
resampling$instantiate(task = task_slangbos)
autoplot(resampling, task_slangbos)

# validation on spatial data splits in training and test sets
#rr = resample(task_slangbos, learner, resampling, store_models = TRUE)
#rr$aggregate(msr("classif.ce"))
#rr$score()$classif.ce %>% median()

# finally train model on all samples
final_model = learner$train(task_slangbos)
# ---------------------------------------
# prediction

dt_ras = as.data.table.raster(ras, xy = TRUE)

# predict on the entire scene
pred = final_model$predict_newdata(task = task_slangbos, newdata = dt_ras)
pred.dt = pred %>% as.data.table()

# wrapper for writing out
pred.geo = georeferencing(prediction = pred.dt, pre_prediction = dt_ras, crs = 32735)
writeRaster(pred.geo, filename = file.path(path, "test.tif"), overwrite = TRUE)

# predict on all the other years

# ---------------------------------------
# function for performing the same anaylsis on the other years

# ---------------------------------------
# MAPPING
dss = list.files(path, pattern = "DataStack.*img$", full.names = TRUE)


# depr
# pivoting data frame (colums "variables" of layers to rows "observations"
# crs = "+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs"
