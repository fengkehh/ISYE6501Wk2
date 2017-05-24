iris_data <- read.table('iris.txt', header = TRUE)

iris_noresp <- iris_data[,-5]
varnames = rep('', 10)

for (k in 1:10) {
  kcl <- kmeans(iris_noresp, centers = k, nstart = 5)
  varnames[k] <- paste('cl', k, sep = '')
  assign(varnames[k], value = kcl)
}

withinss <- rep(0, 10)

for (i in 1:10) {
  withinss[i] = get(varnames[i])$tot.withinss
}

plot(withinss)