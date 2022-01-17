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
# Coercing large rasters to data.tables ----------------------------------------
################################################################################

#' Transform raster to data.table
#'
#' @param x  Raster* object
#' @param row.names	`NULL` or a character vector giving the row names for the data frame. Missing values are not allowed
#' @param optional	logical. If `TRUE`, setting row names and converting column names (to syntactic names: see make.names) is optional
#' @param xy  logical. If `TRUE`, also return the spatial coordinates
#' @param centroids	logical. If TRUE return the centroids instead of all spatial coordinates (only relevant if xy=TRUE)
#' @param sepNA	logical. If TRUE the parts of the spatial objects are separated by lines that are NA (only if xy=TRUE and, for polygons, if centroids=FALSE
#' @param ...	 Additional arguments (none) passed to `raster::as.data.frame`
#'
#' @value returns a data.table object
#' @examples
#' logo <- brick(system.file("external/rlogo.grd", package="raster"))
#' v <- as.data.table(logo)
#' @import
#'
#' credits: https://gist.github.com/etiennebr/9515738, etiennebr/as.data.table.r

as.data.table.raster <- function(x, row.names = NULL, optional = FALSE, xy=FALSE, inmem = canProcessInMemory(x, 2), ...) {
    stopifnot(require("data.table"))
    if(inmem) {
        v <- as.data.table(as.data.frame(x, row.names=row.names, optional=optional, xy=xy, ...))
        coln <- names(x)
        if(xy) coln <- c("x", "y", coln)
        setnames(v, coln)
    } else {
        tr <- blockSize(x)
        l <- lapply(1:tr$n, function(i) {
            DT <- as.data.table(as.data.frame(getValues(x, row = tr$row[i], nrows = tr$nrows[i]), ...))
            if(xy == TRUE) {
                cells <- cellFromRowCol(x, c(tr$row[i], tr$row[i] + tr$nrows[i] - 1), c(1, ncol(x)))
                coords <- xyFromCell(x, cell = cells[1]:cells[2])
                DT[, c("x", "y") := data.frame(xyFromCell(x, cell = cells[1]:cells[2]))]
            }
            DT
        })
        v <- rbindlist(l)
        coln <- names(x)
        if(xy) {
            coln <- c("x", "y", coln)
            setcolorder(v, coln)
        }
    }
}


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
