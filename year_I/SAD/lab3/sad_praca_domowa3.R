veneer <- read.csv("veneer.csv")
a = 0.95
min_value = 2.0

brand_evaluate_hypothesis <- function (data) {
  n <- length(data)
  quanti <- qt(0.5 + a/2, n - 1)
  m <- mean(data)
  S <- sd(data)
  # i teraz zależy jak interpretować nadawanie się do
  # sprzedaży. Przyjmuję, że P (u_0 > 2) > a jako
  # walidację hipotezy
  # Wykomentowane liczenie przedziału pewności 'a' procent,
  # scentrowanego na średniej. Ale dosłowne uzycie testu
  # w tym zadaniu chyba nie ma sensu.
  
  # m - quanti * S / sqrt(n) > min_value
  pt((m - min_value) / S * sqrt(n - 1), n-1) > a
}

tapply(veneer$wear, veneer$brand, brand_evaluate_hypothesis)

# Wyniki porównywałem z następującym kodem:
# Używa on sqrt(n) zamiast sqrt(n-1)

#tapply(veneer$wear, veneer$brand, function (data) {
#  t.test(data)
#})
