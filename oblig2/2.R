library(tidyverse)
set.seed(1110)

n    <- 31
meanA <- 93.32
sdA   <- 15.41
meanB <- 96.58
sdB   <- 13.84
meanD <- -3.26
sdD   <- 8.81

# a)

# b)
t_obs <- meanD / (sdD / sqrt(n))
df    <- n - 1
pval  <- 2 * (1 - pt(abs(t_obs), df))

c(t_verdi = t_obs,
  frihetsgrader = df,
  p_tosidig = pval)

# c)
t_crit <- qt(0.975, df)

ci_low  <- meanD - t_crit * sdD / sqrt(n)
ci_high <- meanD + t_crit * sdD / sqrt(n)

c(ci_95_lower = ci_low,
  ci_95_upper = ci_high)

c(se_beregnet = sdD / sqrt(n),
  se_gitt = 1.58)
