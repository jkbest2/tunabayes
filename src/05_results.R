library(rstan)
library(bayesplot)
library(tidyverse)

##-Model fits-------------------------------------------------------------------

fit_datetime <- "2018-11-13 05:22:19"
load(paste0("results/", fit_datetime, "_fits.Rdata"))

post_df <- fit_df %>%
  transmute(model_name = model_name,
            post = map(fit, extract, inc_warmup = FALSE))

diag_df <- fit_df %>%
  mutate(ess = map(fit, ~ summary(.)$summary[, "n_eff"])) %>%
  transmute(model_name = factor(model_name, unique(model_name)),
            td_total = map_dbl(fit, get_num_max_treedepth),
            div_total = map_dbl(fit, get_num_divergent),
            ## na.rm to get rid of fixed (Pmed[1]) pars with NaN ESS
            exp_ess = map_dbl(ess, mean, na.rm = TRUE),
            min_ess = map_dbl(ess, min, na.rm = TRUE),
            min_ess_par = map_chr(ess,
                                  function(p) attr(p, "names")[which.min(p)]),
            max_ess = map_dbl(ess, max, na.rm = TRUE))

pdiag <- diag_df %>%
  select(model_name, td_total, div_total)
pdiag2 <- pdiag[c(1, seq(2, 13, 2)), ]
pdiag2$td_adj <- c(NA, pdiag$td_total[seq(3, 13, 2)])
pdiag2$div_adj <- c(NA, pdiag$div_total[seq(3, 13, 2)])
pdiag2 <- select(pdiag2,
                 ` ` = model_name,
                 `Exc Treedepth` = td_total,
                 `Exc Treedepth Adj` = td_adj,
                 `Divergences` = div_total,
                 `Divergences Adj` = div_adj)

present_diag_df <- diag_df %>%
  select(` ` = model_name,
         `edepth` = td_total,
         `Divergences` = div_total)
present_diag_df <- present_diag_df[c(1, seq(2, 13, 2)), ]
present_diag_df$`Adj Max Treedepth` <- c(NA, present_diag_df$`Exc Max Treedepth`[seq(3, 13, 2)])

time_df <- fit_df %>%
  mutate(model_name = factor(model_name, unique(model_name))) %>%
  left_join(diag_df, by = "model_name") %>%
  mutate(times = map(fit, get_elapsed_time)) %>%
  transmute(model_name = model_name,
            warmup = map_dbl(times, ~ sum(.[, "warmup"])),
            sample = map_dbl(times, ~ sum(.[, "sample"])),
            min_ess_rate = min_ess / sample,
            exp_ess_rate = exp_ess / sample)

time_unadj <- time_df[c(1, seq(2, 13, 2)), ]
knitr::kable(time_unadj)

time_adj <- time_df[seq(3, 13, 2), ]
knitr::kable(time_adj)

##-Short series fits------------------------------------------------------------

shortfit_datetime <- "2018-11-13 05:45:55"
load(paste0("results/", shortfit_datetime, "_shortfits.Rdata"))

diag_df <- short_df %>%
  mutate(ess = map(fit, ~ summary(.)$summary[, "n_eff"])) %>%
  transmute(model_name = factor(model_name, unique(model_name)),
            T = T,
            td_total = map_dbl(fit, get_num_max_treedepth),
            div_total = map_dbl(fit, get_num_divergent),
            ## na.rm to get rid of fixed (Pmed[1]) pars with NaN ESS
            exp_ess = map_dbl(ess, mean, na.rm = TRUE),
            min_ess = map_dbl(ess, min, na.rm = TRUE),
            min_ess_par = map_chr(ess,
                                  function(p) attr(p, "names")[which.min(p)]),
            max_ess = map_dbl(ess, max, na.rm = TRUE))


time_df <- short_df %>%
  mutate(model_name = factor(model_name, unique(model_name))) %>%
  left_join(diag_df, by = "model_name") %>%
  mutate(times = map(fit, get_elapsed_time),
         model_name = factor(model_name, levels = unique(diag_df$model_name))) %>%
  transmute(model_name = model_name,
            T = T,
            warmup = map_dbl(times, ~ sum(.[, "warmup"])),
            sample = map_dbl(times, ~ sum(.[, "sample"])),
            min_ess_rate = min_ess / sample,
            exp_ess_rate = exp_ess / sample)

shortdivs <- diag_df %>%
  select(model_name, T, div_total) %>%
  spread(key = T, value = div_total) %>%
  arrange(model_name) %>%
  rename(` ` = model_name)

noadj_shortdivs <- shortdivs[c(1, seq(2, 13, 2)), ]
knitr::kable(noadj_shortdivs)

adj_shortdivs <- shortdivs[seq(3, 13, 2), ]
knitr::kable(adj_shortdivs)
