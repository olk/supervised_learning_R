library(dplyr)
library(ggplot2)
library(magrittr)
library(mlbench)
library(randomForest)

data(cars)
data(BreastCancer)

rmse <- function(x,t) {
    sqrt(mean(sum((t-x)^2)))
}

model <- cars %>%
    randomForest(dist ~ speed, data=.)

rmse(predict(model, cars), cars$dist)


model <- BreastCancer %>%
    randomForest(Class ~ Cl.thickness, data=.)

predict(model, BreastCancer) %>%
    head

# confusion matrix
table(BreastCancer$Class, predict(model, BreastCancer))
