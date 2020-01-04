data <- read.csv('wine.csv')
wine.class <- data[, 1]
wine.data <- data[, -c(1)]

hist(wine.class)
wine.factors <- factor(wine.class)
nclass <- length(levels(wine.factors))

# Czy przemnożenie współczynnika regresji prez stosowną wartość
# nie da tego samego efektu (pomijając overflow)?
wine.data <- scale(wine.data)

TRAIN_SIZE <- 4400
train.idx <- sort(sample(1:nrow(wine.data), TRAIN_SIZE))
test.idx <- setdiff(c(1:nrow(wine.data)), train.idx)

wine.train.data <- wine.data[train.idx]
wine.test.data <- wine.data[test.idx]
wine.train.class <- wine.class[train.idx]
wine.test.class <- wine.class[test.idx]
