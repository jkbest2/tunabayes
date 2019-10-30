library(tidyverse)

## Load data frames of diagnostics
fullPT_diagnostics <- readRDS("results/fullPT_diagnostics.Rds") %>%
  mutate(dyn = factor("P-T, estimated m", levels = dyn_levels))
fixedPT_diagnostics <- readRDS("results/fixedPT_diagnostics.Rds") %>%
  mutate(dyn = factor("P-T, fixed m", levels = dyn_levels))
Schaefer_diagnostics <- readRDS("results/Schaefer_diagnostics.Rds") %>%
  mutate(dyn = factor("Schaefer", levels = dyn_levels))

## And in the darkness bind them
all_diagnostics <- bind_rows(fullPT_diagnostics,
                             fixedPT_diagnostics,
                             Schaefer_diagnostics) %>%
  select(dyn, model_name, adapt_delta, div_total, div, summary, ess_rate)

chosen_diagnostics <- all_diagnostics %>%
    group_by(dyn, model_name) %>%
    arrange(div_total, desc(ess_rate), .by_group = TRUE) %>%
    slice(1)

Rhat_df <- chosen_diagnostics %>%
  mutate(Rhat = map(summary, ~ .x$summary[, "Rhat"]),
         max_Rhat = map_dbl(Rhat, max, na.rm = TRUE),
         min_Rhat = map_dbl(Rhat, min, na.rm = TRUE))
