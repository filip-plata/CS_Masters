# Nie jestem pewien czy to było oczekiwane,
# gdyż zadanie 2 zostało w zasadzie zrobione.
# Wygładziłem więc kod, popatrzyłem na wykresy i
# napisałem na końcu wyjaśnienie.

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

plot(ns, mean_data)
plot(ns, median_data)

# Widać, że średnia estymatorów ma bias. Wystarczy popatrzeć
# na wzór: suma/(n - 1). Niezbiasowanym estymatorem byłoby
# suma / (N*n - 1), natomiast użycie średniej estymatorów daje
# suma / ((n - 1) * N), co podnosi wykres w górę.
# Estymator jest zgodny, gdyż czynnik mnożenia zbiega do 1
# przy n dążącym do nieskończoności.

# Mediana nie wydaje się mieć biasu. Ciężko przeliczyć, gdzie
# estymator wariancji ma kwantyl 0.5
