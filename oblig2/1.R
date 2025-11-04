library(tidyverse)
set.seed(1110)

d <- read.table("data/kveite_for.txt", sep = "", header = TRUE) %>%
  as_tibble() %>%
  mutate(Fortype = as.factor(Fortype))

# a)
plot_a <- d %>%
  ggplot(aes(x = Fortype, y = Vekt, fill = Fortype)) +
  geom_boxplot() +
  theme_bw()

ggsave("plots/oppg1_a.pdf", plot_a, width = 7, height = 4)

# b)
plot_b <- d %>%
  ggplot(aes(sample = Vekt)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Fortype) +
  theme_bw() +
  labs(x = "Teoretiske kvantiler", y = "Observerte kvantiler")

ggsave("plots/oppg1_b.pdf", plot_b, width = 7, height = 4)

# c)
A <- d$Vekt[d$Fortype == "A"]
B <- d$Vekt[d$Fortype == "B"]

# d)
sp2 <- ((var(A) * (length(A) - 1) + var(B) * (length(B) - 1)) /
          (length(A) + length(B) - 2))

t_obs <- (mean(B) - mean(A)) /
  sqrt(sp2 * (1/length(A) + 1/length(B)))

df <- length(A) + length(B) - 2
p_manual <- 1 - pt(t_obs, df)

c(t_verdi = t_obs,
  frihetsgrader = df,
  p_ensidig = p_manual)

t.test(B, A, var.equal = TRUE, alternative = "greater")

# e)
t.test(B, A, var.equal = FALSE, alternative = "greater")
var.test(B, A)

# f)
f <- d %>% mutate(x = ifelse(Fortype == "B", 1, 0))
fit <- lm(Vekt ~ x, data = f)
summary(fit)
