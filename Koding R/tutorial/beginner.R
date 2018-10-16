iris <- read.csv("iris.data", header = FALSE)
##iris <- data[,apply(iris), 2, function(x) all(is.na(x))]

##delete misval
row.has.na <- apply(iris, 1, function(x){any(is.na(x))})
sum(row.has.na)
iris.filtered <- iris[!row.has.na,]

iris[is.na(iris)] <- 0

library(ggplot2)
ggplot(data.frame(iris), aes(x = 1:nrow.(iris), y = 'V1')) + geom_line()
ggplot(data.frame(iris[1:100,]), aes(x = 1:100, y = 'V1')) + geom_line()