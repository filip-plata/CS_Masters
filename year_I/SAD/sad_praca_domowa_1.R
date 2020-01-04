# get random mój własny, trochę gorszej
# jakości niż ten z ćwiczeń

get_random <- function(N, sample_size, distribution, ...) {
  result <- lapply(c(1:N), function(x) {
    return(mean(distribution(sample_size, ...)))
    })
  return(unlist(result))
}

gamma_randoms <- sort(get_random(1000, 10, rgamma, 1))
#red
hist(gamma_randoms, col = "blue", freq = FALSE)
true_gamma <- dgamma(x, 10, rate = 10)
#violet
lines(x, true_gamma,col = "red")

# na kilka uruchomień wykresy w przybliżeniu się pokrywały

