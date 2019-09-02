## Use `furrr` to parallelize postprocessing
library(furrr)
library(tidyverse)
library(rstan)

## Automatically choose multicore on Linux or Mac and multisession on Windows.
## If problems are encountered, e.g. objects too large to process in parallel,
## use `plan(sequential)`.
## plan(sequential)
plan(multiprocess)

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
    mutate(post = future_map(fit, as.data.frame),
           model_name = model_name,
           adapt_delta = adapt_delta,
           post_means = future_map(post, ~ lapply(., mean)),
           post_qtiles = future_map(post, post_qtiles)) %>%
    select(-model, -fit, -post)
}

## Extract diagnostics from fits
diagnose_fits <- function(df_fits) {
  df_fits %>%
    mutate(post = future_map(fit, as.array),
           ess = purrr::future_map(fit, ~ summary(.)$summary[, "n_eff"]),
           ess_tail = future_map(post,
                          ~ apply(., 3, ess_tail)),
           ess_bulk = future_map(post,
                          ~ apply(., 3, ess_bulk)),
           div_total = future_map_dbl(fit, get_num_divergent),
           div = div_total > 0,
           td_total = future_map_dbl(fit, get_num_max_treedepth),
           ## na.rm to get rid of fixed (Pmed[1]) pars with NaN ESS
           min_ess = future_map_dbl(ess, min, na.rm = TRUE),
           min_ess_tail = future_map_dbl(tail),
           times = purrr::future_map(fit, get_elapsed_time),
           sampling_time = future_map_dbl(times, ~ sum(times[, 2])),
           ## Calculate the number of effectively independent samples per
           ## second; our primary measure of efficiency
           ess_rate = min_ess / sampling_time,
           ess_tail_rate = min_ess_tail / sampling_time,
           ess_bulk_rate = min_ess_bulk / sampling_time) %>%
    select(-model, -fit, -post, -times)
}

