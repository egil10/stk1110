

library(tidyverse)

# a)

x <- scan("data/forsikringskrav.txt")
xb <- mean(x)
s2 <- mean((x-xb)^2)
alpha <- xb^2/s2
gamma <- xb/s2
beta <- s2/xb
xb
s2
alpha
gamma
beta

# b)

n <- length(x)
logL <- n*alpha*log(gamma)-n*lgamma(alpha)+(alpha-1)*sum(log(x))-gamma*sum(x)

n
logL

# d)

x <- scan("data/forsikringskrav.txt")
xb <- mean(x)
s2 <- mean((x - xb)^2)
alpha0 <- xb^2/s2
gamma0 <- xb/s2

negloglikgamma <- function(logalpha, x = x)
{
  n <- length(x)
  alpha <- exp(logalpha)
  gamma <- alpha / mean(x)  # MLE for gamma gitt alpha
  logL <- n*alpha*log(gamma) - n*lgamma(alpha) +
    (alpha - 1)*sum(log(x)) - gamma*sum(x)
  -logL                       # return??r negativ logL
}

fit.ml <- optim(log(alpha0), negloglikgamma, x = x, method = "BFGS")

alpha_mle <- exp(fit.ml$par)
gamma_mle <- alpha_mle / xb
logL_mle  <- -fit.ml$value

alpha_mle
gamma_mle
logL_mle
logL

# e)

set.seed(1705)
B <- 2000
boot <- replicate(B, {
  xb_ <- mean(xb <- sample(x, n, TRUE))
  s2_ <- mean((xb - xb_)^2)
  a0  <- xb_^2 / s2_
  a   <- exp(optim(log(a0), negloglikgamma, x = xb, method = "BFGS")$par)
  c(alpha = a, gamma = a / xb_)
})

alpha_se <- sd(boot["alpha", ])
gamma_se <- sd(boot["gamma", ])
alpha_ci <- quantile(boot["alpha", ], c(.025, .975))
gamma_ci <- quantile(boot["gamma", ], c(.025, .975))

alpha_se
gamma_se
alpha_ci
gamma_ci

# f)

set.seed(1705) 
B <- 2000
mu_hat <- mean(x)
mu_boot <- replicate(B, mean(sample(x, n, TRUE)))
mu_ci <- quantile(mu_boot, c(.025, .975))

mu_hat
mu_ci

# decisions via CI inclusion of 25
rej_05 <- !(25 >= mu_ci[1] & 25 <= mu_ci[2])
rej_01 <- !(25 >= quantile(mu_boot, c(.005, .995))[1] &
              25 <= quantile(mu_boot, c(.005, .995))[2])
rej_05
rej_01








