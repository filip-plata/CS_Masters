library(MASS)

# ID i class są niepotrzebne. Niestety, w notacji
# z '-' był błąd
columns <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")
data <- na.omit(biopsy)
data <- data[, columns]

data.X <- matrix(unlist(data[, -c(2)]), ncol=8, byrow=T)
data.Y <- matrix(unlist(data[, c(2)]), ncol=1, byrow=T)

# D razy robimy Monte Carlo. Wyniki zapisujemy MSE w errs.
# Następnie tworzymy histogram rezultatów.

N <- nrow(data.X)
K <- 10
# Dla 1000 obliczenia potrwają kilka sekund
D <- 1000
errs <- rep(0, D)

for (i in 1:D) {
  tr_idx = sample(N, N - round(N / K))
  data.train.X <- data.X[tr_idx,]
  data.train.Y <- data.Y[tr_idx]
  
  data.test.X <- data.X[-tr_idx,]
  data.test.Y <- data.Y[-tr_idx]
  
  df <- data.frame(x = data.train.X, y = data.train.Y)
  df_test <- data.frame(x = data.test.X, y = data.test.Y)
  model <- lm(y ~ ., data = df)
  errs[i] = mean((predict(model, df_test) - data.test.Y) ^ 2)
}

hist(errs)
