#' Title     : Dry Ecosystems - Slangbos encroachment mapping in South Africa
#' Subtitle  : Introduction to mlr³ as framework for modelling, assessing machine learning algorithms in the context of Earth Observation
#' Project   : EO College
#' Created by: Konstantin Schellenberg
#' University of Jena, Institute for Geography, Department for Earth Observation
#' Created on: 04.01.2022
#' Created for: EO-College - Dry Ecosystems
#' Scrip No. 0 - helper functions and paths

# basic paths needed for the tutorial
path = "./"
path_data = file.path(path, "data")
path_results = file.path(path, "data", "results")
path_datacube = file.path(path, "data", "cube")

################################################################################
# for export of classification product
################################################################################

georeferencing = function(prediction, vec_coords, crs){

    # bind coords on data.table
    pred_coords = cbind(prediction, x = vec_coords$x, y = vec_coords$y)
    vec = terra::vect(pred_coords, geom = c("x", "y"))
    xmin = min(pred_coords$x)
    xmax = max(pred_coords$x)
    ymin = min(pred_coords$y)
    ymax = max(pred_coords$y)

    r <- terra::rast(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, crs=sprintf("epsg:%s", crs), resolution=30)
    vars = names(prediction)[3:length(names(prediction))]
    rasterized = purrr::map(vars, function(var){
        # print(var)
        terra::rasterize(vec, r, field = var)
    })
    merge = terra::rast(rasterized)
    names(merge) = vars
    return(merge)

}
