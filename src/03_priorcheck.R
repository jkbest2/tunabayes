priorfit <- stan('priorcheck.stan', algorithm = "Fixed_param")
monitor(priorfit, probs = c(0.1, 0.9), digits_summary = 2)

lognorm_fit <- stan("src/models/lognormalcheck.stan",
                    chains = 10L, iter = 1e4, warmup = 0L,
                    algorithm = "Fixed_param")
post <- as.data.frame(lognorm_fit, pars = c("X", "Y"))

post %>%
  gather("par", "val") %>%
  ggplot(aes(x = val, fill = par, color = par)) +
  geom_density(alpha = 0.4)
