library(dplyr)
library(ggplot2)
library(magrittr)
library(mlbench)
library(rpart)
library(party)

data(cars)
data(BreastCancer)

rmse <- function(x,t) {
    sqrt(mean(sum((t-x)^2)))
}

model <- cars %>%
    rpart(dist ~ speed, data=.)
rmse(predict(model, cars), cars$dist)

model <- BreastCancer %>%
    rpart(Class ~ Cl.thickness, data=.)

predict(model, BreastCancer) %>%
    head

predicted_class <- predict(model, BreastCancer) %>%
    as.data.frame %$%
    ifelse(benign > 0.5, "benign", "malignant")

# confusion matrix
table(BreastCancer$Class, predicted_class)


model <- cars %>%
    ctree(dist ~ speed, data=.)
rmse(predict(model, cars), cars$dist)

model <- BreastCancer %>%
    ctree(Class ~ Cl.thickness, data=.)

predict(model, BreastCancer) %>%
    head

table(BreastCancer$Class, predict(model, BreastCancer))

cars %>%
    ctree(dist ~ speed, data=.) %>%
    plot
