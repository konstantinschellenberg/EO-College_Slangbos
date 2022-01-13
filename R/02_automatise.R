#' Title     : Dry Ecosystems - Slangbos encroachment mapping in South Africa
#' Subtitle  : Introduction to mlr³ as framework for modelling, assessing machine learning algorithms in the context of Earth Observation
#' Project   : EO College
#' Created by: Konstantin Schellenberg
#' University of Jena, Institute for Geography, Department for Earth Observation
#' Created on: 04.01.2022
#' Created for: EO-College - Dry Ecosystems
#' Scrip No. 2

# required packages for this tutorial:
requirements = c("tidyverse", "raster", "sf", "mlr3", "mlr3spatiotempcv",
                 "mlr3learners", "ranger", "exactextractr", "kknn")

# load en-block:
sapply(requirements, require, character=TRUE)

# -----------------------------------
# Automatise for all years (operational modelling)

# Load data
source("./R/_helpers.R")

# load dataset of the first year (2015)
rasters.in = list.files(path_datacube, pattern = "DataStack.*\\d{4}.*\\.img$", full.names = TRUE)
layernames.in = list.files(path_datacube, pattern = "^Lay.*\\d{4}", full.names = TRUE)

# vectorise reading in
layernames = map(layernames.in, ~ read_csv(.x, show_col_types = FALSE))
rasters = map2(rasters.in, layernames, function(x, y) {
    ras = brick(x)
    names(ras) = y$layer
    return(ras)
})

# no. of datasets
length(rasters)

# no. of bands in each dataset
map_dbl(rasters, ~ raster::nbands(.x))

# no. of pixels in spatial dimensions and total no. of cells
map_dbl(rasters, ~ raster::ncol(.x))
map_dbl(rasters, ~ raster::nrow(.x))
map_dbl(rasters, ~ raster::ncell(.x))

# -----------------------------------
# wrangle training/test samples
lc = read_sf(file.path(path_data, "Features.gpkg"), layer = "samples")

lc_sfs = map(rasters, function(r){
    # the steps are explained in 02_modelling
    lc_dfs = exact_extract(r, lc,
                           include_cols = c("slangbos", "class"),
                           include_xy = TRUE) %>%
        dplyr::bind_rows() %>%
        dplyr::filter(coverage_fraction >= 0.5) %>%
        dplyr::select(-coverage_fraction) %>%
        mutate_at(vars("slangbos", "class"), as.factor)
    lc_sf = sf::st_as_sf(lc_dfs, coords = c("x", "y"))
})

# Binary
lc_binary = map(lc_sfs, ~ dplyr::select(.x, -class))
map(lc_binary, ~ map_df(.x, ~ class(.x)))

# Multiclass:
lc_multiclass = map(lc_sfs, ~ dplyr::select(.x, -slangbos))

# -----------------------------------
stack = rasters[[1]]
trainingset = lc_binary[[1]]
learner_id="classif.ranger"; target_variable="slangbos"
crs = 32735
outfile = "/home/c3urma/Projects/EO-College_Slangbos/data/results/DataStackSubset2017Prediction"
hyperparameters = list(importance = "impurity",num.trees = 500)

model_and_save = function(stack, trainingset, outfile, learner_id="classif.ranger", target_variable="slangbos",
                          hyperparameters, crs=32735){
    cat("# -------------------- TASK ------------------------")
    task = TaskClassifST$new(id = "Automate", backend = trainingset, target = target_variable)
    print(task)

    cat("\n# -------------------- LEARNER ------------------------")
    learner = lrn(learner_id, predict_type = "prob")
    print(learner)
    prop1 = str_replace(learner_id, "\\.", "")

    cat("\n# -------------------- HYPERPARAMETERS ------------------------")
    learner$param_set$values = hyperparameters
    mtry = ceiling(sqrt(task$ncol))
    learner$param_set$values[["mtry"]] = mtry
    print(as.data.table(learner$param_set$values))
    cat("... rest remains default")

    cat("\n# -------------------- TRAIN ------------------------")
    model = learner$train(task)
    print(learner$model)

    cat("\n# -------------------- PREDICT ------------------------")
    newdata = as.data.table.raster(stack, xy = TRUE)
    prediction_dt = model$predict_newdata(task = task, newdata = newdata) %>% as.data.table()
    prediction = georeferencing(prediction = prediction_dt, pre_prediction = newdata, crs = crs)

    cat("\n# -------------------- SAVE ------------------------")
    writeRaster(prediction, format = "GTiff", filename = paste(outfile, prop1, sep = "_"),
                overwrite = TRUE)
    return(prediction)

}

old_files = str_split(rasters.in, "/|\\.")  # inherently vectorised function
# create new file names by concatinating on the old
new_fileendings = map(old_files, ~ paste(.x[length(.x) - 1], "Prediction", sep = ""))
# new complete file
new_files = map(new_fileendings, ~ file.path(path_results, .x))

# hyperparameters
num.trees = 300

# model one year
prediction2015 = model_and_save(rasters[[1]], lc_binary[[1]], outfile = new_files[[1]],
                                hyperparameters = list(importance = "impurity",
                                                       num.trees = num.trees))

# iterate over all
predictions = pmap(list(rasters, lc_binary, new_files), function(raster, lc, name){
    model_and_save(raster, lc, outfile = name,
                   hyperparameters = list(importance = "impurity",
                                          num.trees = num.trees,
                                          mtry = mtry)
    )
    print(sprintf("Model prediction saved to: %s", name))
})

# -----------------------------------
# Task:
#' Run performance estimations for the shrub detection model for each year. Use pipeline introduced in
#' the previous script 02_modelling as guideline. You can choose the performance measure you prefer
#' (e.g. permutation-based importance, classification accuracy/error, AUROC, OOB prediction error)
#' Try writing the functions the can be access in a vectorsied way, e.g. for all years iteratively.

# FUNCTION
performance_estimation = function (){
}

# VECTORISATION
map(rasters, ~ performance_estimation())

# -----------------------------------
# --> next script 03_visualisation for visual inspection of the results
