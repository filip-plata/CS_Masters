library(ISLR)
library(rpart)

planets <- read.csv("planets.csv", header = TRUE, sep = "\t")
specimens <- read.csv("specimens.csv", header = TRUE, sep = " ")
alien <- read.csv("alien.txt", header = TRUE, sep = " ")
data <- merge(planets, specimens, by="planet")

tree <- rpart(species ~ .-planet, data = data)
plot(tree)
text(tree)

spec_tree <- rpart(species ~ ., data = specimens[,2:length(specimens)])
predict(spec_tree, alien, type = "class")

# Wychodzi a1
