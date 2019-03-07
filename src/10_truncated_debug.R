library(rstan)
library(tidyverse)

source("src/01_setup.R")
source("src/02_fitfns.R")

chain_spec <- list(n_iter = 2e4,
                   n_warm = 5e3,
                   n_chain = 6L)

adj_ad <- list(adapt_delta = 0.975)

trunc_mods <- tibble(model = c("truncated", "truncated q", "centered", "centered_precision"),
                     file = c("src/models/00_truncated.stan",
                              "src/models/000_truncated.stan",
                              "src/models/01_centered.stan",
                              "src/models/100_centered_original.stan"))

trunc_fits <- trunc_mods %>%
  mutate(fit = map(file,
                   stan,
                   data = tuna_data,
                   chains = 6L,
                   iter = 4000L,
                   warmup = 2000L,
                   init = init_cent,
                   control = adj_ad))

trunc_fits %>%
  transmute(model = model,
            q = map(fit, rstan::extract, pars = "sigma")) %>%
  unnest() %>% unnest() %>%
  group_by(model) %>%
  summarize(p025 = quantile(q, 0.025),
            p10 = quantile(q, 0.1),
            p25 = quantile(q, 0.25),
            p50 = quantile(q, 0.5),
            p75 = quantile(q, 0.75),
            p90 = quantile(q, 0.9),
            p975 = quantile(q, 0.975)) %>%
  gather(qt, val, -model) %>%
  ggplot(aes(x = val, y = model)) +
  geom_point()

co_fit <- stan("src/models/100_centered_original.stan", data = tuna_data, init = init_cent)
