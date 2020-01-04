shad <- read.csv("shad.csv")
veneer <- read.csv("veneer.csv")

wear <- veneer[veneer$brand == "CHAMP", "wear"]
wear_mean = mean(wear)
var_est_biased <- mean((wear - wear_mean) ^ 2)
var_est_unbiased <- var_est_biased * length(wear)/(length(wear) - 1)

est_std <- sqrt(var_est_unbiased)

sd(wear)
var(wear)

#2

sampler <- function(N, n, dist, est, ...) {
  vals <- dist(N * n, ...)
  d <- matrix(vals, nrow = N, ncol = n)
  apply(d, 1, est)
}

boxplot(sampler(1000, 10, rnorm, var, 0, 1))

N <- 1000
ns <- seq(10, N, by = 10)
vector_data <- sapply(ns, function (n) {sampler(N, n, rnorm, var)})
mean_data <- apply(vector_data, 2, mean)
median_data <- apply(vector_data, 2, median)

# zadanie 2 jako praca domowa

plot(ns, mean_data)

# 3

tapply(shad$length, shad$site, median)
library(MASS)
data(topo)
qqnorm(topo$z)
a = 0.01
n = length(topo$z)
m = mean(topo$z)
l = qt(1 - a/2, df=n-1) / sqrt(n-1) * sd(topo$z)
lq <- qnorm(1 - a/2) * sd(topo$z) / sqrt(n)
student_interval <- c(m - l, m + l)
asymptotic_interval <- c (m - lq, m + lq)
diff(l, lq)

# Estimating uniform distribution

N = 1000
n = 10
b = 1
unif = runif(N * n, 0, b)
m <- matrix(unif, nrow = N, ncol = n)
a = 0.95

sum(apply(m, 1, function (x) {  1 <= max(x) / a^(1/n) }))

# naklepać chi_kwadrat test
# praca domowa lab4 jednopróbkowy test studenta