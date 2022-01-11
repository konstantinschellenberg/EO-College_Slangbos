# Title     : TODO
# Created by: c3urma
# Created on: 11.01.22

# required packages for this tutorial:
requirements = c("tidyverse", "raster", "sf", "mlr3", "mlr3spatiotempcv",
                 "mlr3learners", "ranger", "exactextractr", "kknn")

# load en-block:
sapply(requirements, require, character=TRUE)

# -----------------------------------
# Automatise for all years (operational modelling)

performance_estimation = function (){

}

performance_estimation()
predict_and_save()

