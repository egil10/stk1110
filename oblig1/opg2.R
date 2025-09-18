
library(tidyverse)

# a)

x <- c(525,587,547,558,591,531,571,551,566,622,561,502,556,565,562)
n <- length(x)
xb <- mean(x)
s <- sd(x)
tcrit <- qt(.975, df = n-1)
lo <- xb - tcrit * s / sqrt(n)
hi <- xb + tcrit * s / sqrt(n)

xb
s
lo
hi

# b)

set.seed(1705)
B <- 10000
n <- 15
mu <- 558
sigma <- 30

cov <- replicate(B, {
  y <- rnorm(n, mu, sigma)
  xb <- mean(y); s <- sd(y)
  tcrit <- qt(.975, df = n - 1)
  lo <- xb - tcrit * s / sqrt(n); hi <- xb + tcrit * s / sqrt(n)
  (mu >= lo) & (mu <= hi)
})

cover <- mean(cov)
se_mc <- sqrt(cover * (1 - cover) / B)  
cover
se_mc

# c)

set.seed(1705)
cov_z <- replicate(B, {
  y <- rnorm(n, mu, sigma)
  xb <- mean(y)
  s <- sd(y)
  lo <- xb - 1.96 * s / sqrt(n)
  hi <- xb + 1.96 * s / sqrt(n)
  (mu >= lo) & (mu <= hi)
})
cover_z <- mean(cov_z)
se_mc_z <- sqrt(cover_z * (1 - cover_z) / B)
cover_z; se_mc_z

# d)

set.seed(1705)
cov_sigma <- replicate(B, {
  y <- rnorm(n, mu, sigma)
  s <- sd(y)
  lo <- sqrt((n - 1) * s^2 / qchisq(.975, df = n - 1))
  hi <- sqrt((n - 1) * s^2 / qchisq(.025, df = n - 1))
  (sigma >= lo) & (sigma <= hi)
})
cover_sigma <- mean(cov_sigma)
se_mc_sigma <- sqrt(cover_sigma * (1 - cover_sigma) / B)
cover_sigma; se_mc_sigma

# e)

set.seed(1705)
cov_t7 <- replicate(B, {
  z <- rt(n, df = 7)
  y <- mu + sigma * z
  xb <- mean(y)
  s <- sd(y)
  tcrit <- qt(.975, df = n - 1)
  lo <- xb - tcrit * s / sqrt(n); hi <- xb + tcrit * s / sqrt(n)
  (mu >= lo) & (mu <= hi)
})
cover_t7 <- mean(cov_t7)
se_mc_t7 <- sqrt(cover_t7 * (1 - cover_t7) / B)
cover_t7; se_mc_t7

# f)

set.seed(1705)
tilde_sigma <- sqrt(1.4) * sigma

cov_t7_sigma <- replicate(B, {
  y <- mu + sigma * rt(n, df = 7)
  s <- sd(y)
  lo <- sqrt((n - 1) * s^2 / qchisq(.975, df = n - 1))
  hi <- sqrt((n - 1) * s^2 / qchisq(.025, df = n - 1))
  (tilde_sigma >= lo) & (tilde_sigma <= hi)
})

cover_t7_sigma <- mean(cov_t7_sigma)
se_mc_t7_sigma <- sqrt(cover_t7_sigma * (1 - cover_t7_sigma) / B)
cover_t7_sigma; se_mc_t7_sigma


