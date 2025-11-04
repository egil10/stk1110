library(tidyverse)
set.seed(1110)

# problem 4

d <- read.table("data/snoe_vann.txt", header = FALSE) %>%
  as_tibble() %>%
  rename(Snoinnhold = V1, Vannstand = V2)


# a)

fit <- lm(Vannstand ~ Snoinnhold, data = d)
summary(fit)

plot_a <- d %>%
  ggplot(aes(x = Snoinnhold, y = Vannstand)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_bw()

ggsave("plots/oppg4_a.pdf", plot_a, width = 7, height = 4)


# b)

res <- residuals(fit)

plot_resid <- ggplot(data.frame(Snoinnhold = d$Snoinnhold, res = res),
                     aes(x = Snoinnhold, y = res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()

ggsave("plots/oppg4_residualer.pdf", plot_resid, width = 7, height = 4)

plot_qq <- ggplot(data.frame(res), aes(sample = res)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw()

ggsave("plots/oppg4_residual_qq.pdf", plot_qq, width = 7, height = 4)


# c)

sigma2_hat <- sum(res^2) / (nrow(d) - 2)
sigma2_hat

confint(fit, "Snoinnhold", level = 0.95)


# d)

beta0 <- coef(fit)[1]
se_b0 <- summary(fit)$coefficients[1, 2]
t_b0 <- beta0 / se_b0
p_b0 <- 2 * (1 - pt(abs(t_b0), df = nrow(d) - 2))
 
c(beta0 = beta0, SE_beta0 = se_b0, t_verdi = t_b0, p_tosidig = p_b0)


# e)

fit2 <- lm(Vannstand ~ poly(Snoinnhold, 2, raw = TRUE), data = d)
fit3 <- lm(Vannstand ~ poly(Snoinnhold, 3, raw = TRUE), data = d)

summary(fit2)
summary(fit3)

AIC(fit, fit2, fit3)
BIC(fit, fit2, fit3)
