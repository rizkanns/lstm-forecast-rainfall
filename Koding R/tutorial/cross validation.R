library(lattice)
library(ggplot2)
library(caret)
data(mtcars)
set.seed(30)

model <- train(mpg ~ hp, mtcars,
               method = "lm",
               trControl = trainControl(
                 method = "cv", number = 10,
                 verboseIter = TRUE
               )
)