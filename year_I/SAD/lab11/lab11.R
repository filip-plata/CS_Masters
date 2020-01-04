library('MASS')
data = Boston

train.set <- sample(1:nrow(data), 60, rep=F)
X.train = data[train.set, ]
X.test = data[-train.set, ]

B <- runif(12, -3, 3)
zeroB <- sample(c(T, F), 12, rep=T)
B[zeroB] <- 0
names(B) <- names(data)

Y <- as.matrix(data) %*% B +rnorm(506, 0, 1)
Y.train <- Y[train.set]
Y.test <- Y[-train.set]