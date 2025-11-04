library(tidyverse)
set.seed(1110)

n_menn    <- 3000
n_kvinner <- 3000
x_menn    <- 486
x_kvinner <- 441

p_menn    <- x_menn / n_menn
p_kvinner <- x_kvinner / n_kvinner
diff_hat  <- p_menn - p_kvinner

# a)
p_pooled <- (x_menn + x_kvinner) / (n_menn + n_kvinner)
se       <- sqrt(p_pooled * (1 - p_pooled) * (1/n_menn + 1/n_kvinner))
z_obs    <- diff_hat / se
pval     <- 2 * (1 - pnorm(abs(z_obs)))

c(z_verdi = z_obs,
  p_tosidig = pval)

# b)
prop.test(c(x_menn, x_kvinner),
          c(n_menn, n_kvinner),
          alternative = "two.sided",
          correct = FALSE)
