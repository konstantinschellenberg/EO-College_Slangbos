# Title     : TODO
# Created by: Konsti
# Created on: 14.01.2022

library("mlr3")
library("mlr3spatial")
library("mlr3learners")

tif = system.file("tif/L7_ETMs.tif", package = "stars")
stack = stars::read_stars(tif)

backend = as_data_backend(stack)
task = as_task_regr(backend, target = "layer.1")

print(task)
learner = lrn("classif.ranger")
set.seed(42)
row_ids = sample(1:task$nrow, 500)
learner$train(task, row_ids = row_ids)

print(learner)

ras = predict_spatial(task, learner, format = "terra")

plot(ras)
names(ras) = "cadmium"

print(ras)