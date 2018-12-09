library(rstan)
library(bayesplot)
library(tidyverse)

## Get ggplot theme
source("src/02_fitfns.R")

##-Model fits-------------------------------------------------------------------

fit_datetime <- "2018-12-05 15:49:31"
load(paste0("results/", fit_datetime, "_fits.Rdata"))

post_df <- fit_df %>%
  transmute(model_name = model_name,
            post = map(fit, rstan::extract, inc_warmup = FALSE))

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
pdiag2 <- pdiag[c(1, seq(2, 10, 2)), ]
pdiag2$td_adj <- c(NA, pdiag$td_total[seq(3, 11, 2)])
pdiag2$div_adj <- c(NA, pdiag$div_total[seq(3, 11, 2)])
pdiag2 <- select(pdiag2,
                 ` ` = model_name,
                 `Exc Treedepth` = td_total,
                 `Exc Treedepth Adj` = td_adj,
                 `Divergences` = div_total,
                 `Divergences Adj` = div_adj)

knitr::kable(pdiag2)

## present_diag_df <- diag_df %>%
##   select(` ` = model_name,
##          `Exc Max Treedepth` = td_total,
##          `Divergences` = div_total)
## present_diag_df <- present_diag_df[c(1, seq(2, 13, 2)), ]
## present_diag_df$`Adj Max Treedepth` <- c(NA, present_diag_df$`Exc Max Treedepth`[seq(3, 13, 2)])

time_df <- fit_df %>%
  mutate(model_name = factor(model_name, unique(model_name))) %>%
  left_join(diag_df, by = "model_name") %>%
  mutate(times = map(fit, get_elapsed_time)) %>%
  transmute(model_name = model_name,
            warmup = map_dbl(times, ~ sum(.[, "warmup"])),
            sample = map_dbl(times, ~ sum(.[, "sample"])),
            min_ess_rate = min_ess / sample,
            exp_ess_rate = exp_ess / sample)

time_unadj <- time_df[c(1, seq(2, 10, 2)), ]
knitr::kable(time_unadj)

time_adj <- time_df[seq(3, 10, 2), ]
knitr::kable(time_adj)

## Posterior densities - MSY
msy_post <- fit_df %>%
  transmute(model_name = factor(model_name, unique(model_name)),
            MSY = map(fit, rstan::extract, pars = "MSY")) %>%
  unnest() %>% unnest()

msy_post %>%
  filter(model_name %in% c("Truncated", "Centered", "Centered adj", "Marginal q", "Marginal q adj", "Explicit F", "Explicit F marg q")) %>%
  ggplot(aes(x = MSY, color = model_name, fill = model_name)) +
  geom_density(alpha = 0.2, size = 2) +
  labs(title = "MSY Posteriors under different parameterizations",
       x = "Maximum Sustainable Yield", y = "Density",
       color = "Model", fill = "Model") +
  plottheme

##-Varying Catch SD-------------------------------------------------------------
csd_datetime <- "2018-12-02 23:38:03"
load(paste0("results/", csd_datetime, "_csd.Rdata"))

## Diagnostics
diag_csd <- csd_df %>%
  mutate(ess = map(fit, ~ summary(.)$summary[, "n_eff"])) %>%
  transmute(catch_sd = catch_sd,
            td_total = map_dbl(fit, get_num_max_treedepth),
            div_total = map_dbl(fit, get_num_divergent),
            ## na.rm to get rid of fixed (Pmed[1]) pars with NaN ESS
            exp_ess = map_dbl(ess, mean, na.rm = TRUE),
            min_ess = map_dbl(ess, min, na.rm = TRUE),
            min_ess_par = map_chr(ess,
                                  function(p) attr(p, "names")[which.min(p)]),
            max_ess = map_dbl(ess, max, na.rm = TRUE))

knitr::kable(diag_csd, digits = 0)

## Fit timing
time_csd <- csd_df %>%
  left_join(diag_csd, by = "catch_sd") %>%
  mutate(times = map(fit, get_elapsed_time)) %>%
  transmute(catch_sd = catch_sd,
            warmup = map_dbl(times, ~ sum(.[, "warmup"])),
            sample = map_dbl(times, ~ sum(.[, "sample"])),
            min_ess_rate = min_ess / sample,
            exp_ess_rate = exp_ess / sample)


## Posterior densities - catches
c_post <- csd_df %>%
  filter(catch_sd < 1) %>%
  transmute(catch_sd = factor(signif(catch_sd, 2), levels = signif(catch_sd, 2)),
            `C_pred[1]` = map(fit, rstan::extract, pars = "C_pred[1]"),
            `C_pred[8]` = map(fit, rstan::extract, pars = "C_pred[8]"),
            `C_pred[16]` = map(fit, rstan::extract, pars = "C_pred[16]"),
            `C_pred[23]` = map(fit, rstan::extract, pars = "C_pred[23]")) %>%
  unnest() %>% unnest() %>%
  gather(c_year, c_est, -catch_sd) %>%
  mutate(c_year = factor(c_year, levels = c("C_pred[1]", "C_pred[8]",
                                            "C_pred[16]", "C_pred[23]")))

c_orig <- data_frame(c_year = factor(c("C_pred[1]", "C_pred[8]",
                                       "C_pred[16]", "C_pred[23]"),
                                     levels = c("C_pred[1]", "C_pred[8]",
                                                "C_pred[16]", "C_pred[23]")),
                     c_obs = tuna_data$C[c(1, 8, 16, 23)])

c_post %>%
  ggplot(aes(x = c_est, color = catch_sd, fill = catch_sd)) +
  geom_density() +
  facet_wrap(~ c_year, scales = "free") +
  geom_vline(aes(xintercept = c_obs), data = c_orig,
             alpha = 0.5, size = 1, linetype = "dashed") +
  labs(title = "Example catch posteriors",
       x = "Catch biomass", y = "Density",
       color = "Catch SD", fill = "Catch SD") +
  plottheme

## Posterior densities - Biomass
b_post <- csd_df %>%
  filter(catch_sd < 4) %>%
  transmute(catch_sd = factor(signif(catch_sd, 2), levels = signif(catch_sd, 2)),
            `Biomass[1]` = map(fit, rstan::extract, pars = "Biomass[1]"),
            `Biomass[8]` = map(fit, rstan::extract, pars = "Biomass[8]"),
            `Biomass[16]` = map(fit, rstan::extract, pars = "Biomass[16]"),
            `Biomass[23]` = map(fit, rstan::extract, pars = "Biomass[23]")) %>%
  unnest() %>% unnest() %>%
  gather(b_year, b_post, -catch_sd) %>%
  mutate(b_year = factor(b_year, levels = c("Biomass[1]", "Biomass[8]",
                                            "Biomass[16]", "Biomass[23]")))

b_post %>%
  ggplot(aes(x = b_post, color = catch_sd, fill = catch_sd)) +
  geom_density(alpha = 0.25, size = 1) +
  facet_wrap(~ b_year, scales = "free") +
  labs(title = "Example biomass posteriors",
       x = "Biomass",
       y = "Density",
       fill = "Catch SD", color = "Catch SD") +
  plottheme

## Posterior - Biomass series
b_series <- csd_df %>%
  filter(catch_sd < 4) %>%
  transmute(`Catch SD` = factor(signif(catch_sd, 2), levels = signif(catch_sd, 2)),
            bseries = map(fit, as.data.frame, pars = "Biomass")) %>%
  unnest() %>%
  gather(b_year, b_post, -`Catch SD`) %>%
  mutate(year = map_dbl(b_year, ~ as.numeric(substr(.x, 9, nchar(.x) - 1L)))) %>%
  group_by(`Catch SD`, year) %>%
  summarize(p025 = quantile(b_post, 0.025),
            p25 = quantile(b_post, 0.25),
            p50 = median(b_post),
            p75 = quantile(b_post, 0.75),
            p975 = quantile(b_post, 0.975))

## b_series %>%
##   ggplot(aes(x = year, y = p50,
##              color = catch_sd, fill = catch_sd)) +
##   geom_ribbon(aes(ymin = p025, ymax = p975), alpha = 0.25, size = 4) +
##   geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.45, size = 4) +
##   geom_line(size = 4)

b_series %>%
  ggplot(aes(x = year, y = p50,
             color = `Catch SD`, fill = `Catch SD`)) +
  geom_linerange(aes(ymin = p025, ymax = p975), size = 1.5, position = position_dodge(width = 0.75)) +
  geom_linerange(aes(ymin = p25, ymax = p75), size = 3, position = position_dodge(width = 0.75)) +
  geom_point(position = position_dodge(width = 0.75), shape = "-", size = 10, color = "black") +
  labs(title = "Biomass posteriors with varying catch errors",
       x = "Year", y = "Biomass") +
  plottheme

## Posterior densities - MSY
msy_post <- csd_df %>%
  filter(catch_sd < 4) %>%
  transmute(catch_sd = factor(catch_sd, levels = catch_sd),
            MSY = map(fit, extract, pars = "MSY")) %>%
  unnest() %>% unnest()

msy_post %>%
  ggplot(aes(x = MSY, color = catch_sd, fill = catch_sd)) +
  geom_density(alpha = 0.2, size = 2)

