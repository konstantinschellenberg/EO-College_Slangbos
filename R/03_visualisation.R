#' Title     : Dry Ecosystems - Slangbos encroachment mapping in South Africa
#' Subtitle  : Introduction to mlr³ as framework for modelling, assessing machine learning algorithms in the context of Earth Observation
#' Project   : EO College
#' Created by: Konstantin Schellenberg
#' University of Jena, Institute for Geography, Department for Earth Observation
#' Created on: 04.01.2022
#' Created for: EO-College - Dry Ecosystems
#' Scrip No. 3

packages = c("tidyverse", "raster", "gganimate")
theme_set(theme_minimal())

# load en-block:
sapply(packages, require, character=TRUE)

# -----------------------------------
# load all prediction products
source("./R/_helpers.R")

# load predictionspath_results
predictions = list.files(path_results, pattern = "Prediction.*\\.tif$", full.names = TRUE)
rasters = map(predictions, ~ brick(.x))

# -----------------------------------
# 1. Binary classification response

# extract only layer 3 (response)
resp = map(rasters, ~ .x[[3]]) %>% stack()
names(resp) = c("2015", "2016", "2017")
plot(resp)

# now let's create indizes over time for a better understand of land cover turnover:
# RECLASSIFY
# val: 2 (TRUE) -> 1
# val: 1 (FALSE) -> NA
resp[resp == 1] = NA
resp[resp == 2] = 1
plot(resp)

# write out
writeRaster(resp, filename = file.path(path_results, "Response"), format = "GTiff", overwrite = TRUE)

# create unique indizes
added = stack(resp[[1]], resp[[2]] + 10, resp[[3]] + 100)
aggregated = sum(added[[1]], added[[2]], added[[3]], na.rm = TRUE)
plot(aggregated)

# write out
writeRaster(aggregated, filename = file.path(path_results, "BinaryResponseIndex"), format = "GTiff", overwrite = TRUE)

# -----------------------------------
# 2. Classification probability

# extract only layer 5 (positive probability for slangbos)
prob = map(rasters, ~ .x[[5]]) %>% stack()
namer = paste0("Slangbos", c("2015", "2016", "2017"))
names(prob) = namer
plot(prob)
writeRaster(prob, filename = file.path(path_results, "Probability"), format = "GTiff", overwrite = TRUE)

# visualisation
# ggplot requires "long format", even from rasters
prob_dt = as.data.table.raster(prob, xy = TRUE)
prob_dt_pivot = prob_dt %>%
    pivot_longer(cols = -c(x, y), names_to = "Year", values_to = "Slangbos_Probability")

# quick representation of the change
ggplot(prob_dt_pivot) +
    geom_raster(aes(x = x, y = y, fill = Slangbos_Probability)) +
    facet_grid(rows = vars(Year)) +
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red")

# split dataset by years to plot independently
prob_yearly = prob_dt_pivot %>% group_by(Year) %>%
    dplyr::group_split()

maps = map(prob_yearly, function(year){
    yr = year$Year %>% unique()
    ggplot(year) +
        geom_raster(aes(x = x, y = y, fill = Slangbos_Probability)) +
        scale_fill_gradient2(low = "blue", mid = "yellow", high = "red") +
        theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
        labs(fill = "Slangbos Probability", title = yr)
})
print(maps)

# export of maps (can be added to gifs using .g. `gganimate`, or via online tools)
walk2(maps, namer, ~ ggsave(.x, filename = file.path(path_results, paste0(.y, ".png")), device = "png",
                     height = 5, width = 5))

# -----------------------------------
# thanks for following!
# Cheers, Konstantin Schellenberg