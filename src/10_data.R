## Data wrangling and map workflow
library(tidyverse)
## Use the `futures` package to distribute model fits
## library(furrr)
## plan(multicore)
## RStan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
## Required for between-chain parallelization for unknown reason (see rstan
## issue 556 on Github)
library(later)
library(bayesplot)

# data: catches, CPUE Index, and number of years
catch <- c(15.9, 25.7, 28.5, 23.7, 25.0, 33.3, 28.2, 19.7, 17.5, 19.3, 21.6,
           23.1, 22.5, 22.5, 23.6, 29.1, 14.4, 13.2, 28.4, 34.6, 37.5, 25.9,
           25.3)
indices <- c(61.89, 78.98, 55.59, 44.61, 56.89, 38.27, 33.84, 36.13, 41.95,
             36.63, 36.33, 38.82, 34.32, 37.64, 34.01, 32.16, 26.88, 36.61,
             30.07, 30.75, 23.36, 22.36, 21.91)
n_years <- 23L
catch_sd <- 0.2

tuna_data <- list(C = catch,
                  I = indices,
                  T = n_years,
                  catch_sd = catch_sd)

