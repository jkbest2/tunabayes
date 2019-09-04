library(tidyverse)
library(rstan)

## Vector of model names in order for setting order as factor
model_levels <- c("Centered", "Noncentered", "Marginal q", "Explicit F",
                  "Explicit F marg q", "Constrained P")

## A function defining the quantiles we want to summarize the posteriors.
post_qtiles <- function(post_df) {
  lapply(post_df,
         function(df)
           quantile(df, probs = c(0.025, 0.05, 0.1, 0.25, 0.5,
                                  0.75, 0.9, 0.95, 0.975)))
}

## Extract posteriors from `stanfit` objects. Need to use package namespaces due
## to conflicts. Use the `future_map` functions to parallelize processing
summarize_posteriors <- function(df_fits) {
  df_fits %>%
    mutate(post = map(fit, as.data.frame),
           summary = map(fit, summary),
           post_means = map(post, ~ map(.x, mean)),
           post_qtiles = map(post, post_qtiles),
           model_name = factor(model_name, levels = model_levels)) %>%
    select(-model, -fit, -post)
}

## Extract diagnostics from fits
diagnose_fits <- function(df_fits) {
  df_fits %>%
    mutate(post = map(fit, as.array),
           summary = map(fit, summary),
           ess = map(summary, ~ .x$summary[, "n_eff"]),
           ess_tail = map(post, ~ apply(.x, 3, ess_tail)),
           ess_bulk = map(post, ~ apply(.x, 3, ess_bulk)),
           div_total = map_int(fit, get_num_divergent),
           div = div_total > 0,
           td_total = map_int(fit, get_num_max_treedepth),
           ## na.rm to get rid of fixed (Pmed[1]) pars with NaN ESS
           min_ess = map_dbl(ess, min, na.rm = TRUE),
           min_ess_bulk = map_dbl(ess_bulk, min, na.rm = TRUE),
           min_ess_tail = map_dbl(ess_tail, min, na.rm = TRUE),
           times = map(fit, get_elapsed_time),
           sampling_time = map_dbl(times, ~ sum(.x[, 2])),
           ## Calculate the number of effectively independent samples per
           ## second; primary measure of efficiency
           ess_rate = min_ess / sampling_time,
           ess_tail_rate = min_ess_tail / sampling_time,
           ess_bulk_rate = min_ess_bulk / sampling_time,
           model_name = factor(model_name, levels = model_levels)) %>%
    select(-model, -fit, -post, -times)
}

