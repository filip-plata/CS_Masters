data <- read.csv('wine.csv')
wine.class <- data[, 1]
wine.data <- data[, -c(1)]

hist(wine.class)
wine.factors <- factor(wine.class)
nclass <- length(levels(wine.factors))

wine.data <- scale(wine.data)

TRAIN_SIZE <- 4400
train.idx <- sort(sample(1:nrow(wine.data), TRAIN_SIZE))
test.idx <- setdiff(c(1:nrow(wine.data)), train.idx)

wine.train.data <- wine.data[train.idx,]
wine.test.data <- wine.data[test.idx,]
wine.train.class <- wine.class[train.idx]
wine.test.class <- wine.class[test.idx]

knn <- function(x, K) {
  closest_idxs <- head(sort(apply(wine.train.data, 
                      1, function(a) dist(rbind(a,x))),
                index.return=TRUE)$ix, K);
  classes <- wine.train.class[closest_idxs]
  tb <- table(classes)
  return(as.integer(names(tb[tb==max(tb)])))
}

# To powinno być prawdą
knn(wine.train.data[1,], 1) == wine.train.class[1]

