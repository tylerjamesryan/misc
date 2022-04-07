# Age Data Generation

N <- 50000
mode_age <- 30
mean_mu <- mode_age + rlnorm(N, 0, 1.3)
min_age <- 20
max_age <- 80
age <- qpois(
  p = runif(
    n = N, 
    min = ppois(q = min_age, lambda = mean_mu), 
    max = ppois(q = max_age, lambda = mean_mu)), 
  lambda = mean_mu
)

hist(age)
table(age)[which(table(age) == max(table(age)))]
