## Load required packages
library(tidyverse)
library(gridExtra)

## Source plot specifics
source("src/02_plotthemes.R")

## Remove unnecessary columns and make long with label for divergences vs
## treedepth and counts
reshape_diags <- function(df_diag) {
  df_diag %>%
    select(model_name, adapt_delta, div = div_total, td = td_total) %>%
    gather(key = "diagnostic", value = number, -model_name, -adapt_delta)
}

dyn_levels = c("P-T, estimated m",
               "P-T, fixed m",
               "Schaefer")

param_levels <- c("Centered", "Noncentered", "Marginal q",
                  "Explicit F", "Explicit F marg q", "Constrained P")

## Specify colors to use
param_colors <- wong_colors[c(2, 3, 4, 6, 7, 8)]
names(param_colors) <- param_levels

## Specify levels of `adapt_delta` used
ad_vals <- c(seq(0.8, 0.95, 0.05), 0.975, 0.99, 0.999)

