library(rstan)
library(bayesplot)
library(tidyverse)

load("results/fits.Rdata")

post_df <- fit_df %>%
  transmute(model_name = model_name,
            post = map(fit, extract, inc_warmup = FALSE))


diag_df <- fit_df %>%
  mutate(ess = map(fit, ~ summary(.)$summary[, "n_eff"])) %>%
  transmute(model_name = model_name,
            td_total = map_dbl(fit, get_num_max_treedepth),
            div_total = map_dbl(fit, get_num_divergent),
            ## na.rm to get rid of fixed (Pmed[1]) pars with NaN ESS
            exp_ess = map_dbl(ess, mean, na.rm = TRUE),
            min_ess = map_dbl(ess, min, na.rm = TRUE),
            min_ess_par = map_chr(ess,
                                  function(p) attr(p, "names")[which.min(p)]),
            max_ess = map_dbl(ess, max, na.rm = TRUE))

time_df <- fit_df %>%
  left_join(diag_df, by = "model_name") %>%
  mutate(times = map(fit, get_elapsed_time)) %>%
  transmute(model_name = model_name,
            warmup = map_dbl(times, ~ sum(.[, "warmup"])),
            sample = map_dbl(times, ~ sum(.[, "sample"])),
            min_ess_rate = min_ess / sample,
            exp_ess_rate = exp_ess / sample)

