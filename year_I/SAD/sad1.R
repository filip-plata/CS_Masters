#1

size <- 1000
vec <- 1:size
s <- sum(vec)
avg = s / size
vec <- vec * 2
c(1,2,3) + c(4,5,6)
vec[vec > 500]

c(1:size)
vec[(c(1:size) %% 5 == 0)] <- 0
vec1 <- c(-2, -1, vec, 2001, 2002)
len <- length(vec1)

#2

N <- 100000
and_divisible <- sum((c(1:N) %% 706 == 0) & (c(1:N) %% 48 == 0)) 
or_divisible <- sum((c(1:N) %% 706 == 0) | (c(1:N) %% 48 == 0)) 

#3

values <- rnorm(20000, 1, 2)
hist(values,xlab = "Weight",col = "yellow",border = "blue")

matrix_form <- matrix(values,nrow = 20,ncol = 10)
summed_rows <- apply(matrix_form, 1, function(x) mean(x))
hist(summed_rows,xlab = "Weight",col = "yellow",border = "blue")

#4

n <- 1000000
random_from_plane <- cbind(runif(n,0,1),runif(n,0,1))
in_unit_circle <- function(point) {return((point[1] - 0.5)^2 + (point[2] - 0.5)^2 <= 0.25)}
pi_estimate <- 4*mean(apply(random_from_plane, 1, in_unit_circle))

#5

get_random <- function(N, sample_size, distribution, ...) {
  # podzielić na macierz a zrobić od razu dużo liczb
  result <- lapply(c(1:N), function(x) {return(mean(distribution(sample_size, ...)))})
  return(unlist(result))
}

#6

gamma_randoms <- get_random(10000, 10, rgamma, 1)
hist(gamma_randoms, xlab = "Weight",col = rgb(1,0,0,0.5),border = "blue")
true_gamma <- rgamma(10000, 10, rate = 10)
hist(true_gamma, xlab = "Weight",col = rgb(0,0,1,0.5),border = "blue", add=T)
box()
