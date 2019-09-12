## Data wrangling and map workflow
library(tidyverse)
## RStan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
## Required for between-chain parallelization for unknown reason (see rstan
## issue 556 on Github)
library(later)

## Don't set a seed; results shouldn't depend on it!

## Source helper functions
source("src/01_functions.R")
## Read in data
source("src/10_data.R")

## Location, scale, and shape parmameters of log skew normal prior on
## Pella-Tomlinson shape parameter `m`. In terms of Pmsy, median 0.4 with 75th
## quantile very close to 0.5. Only used in the "fullPT" fits (others use a
## fixed `m`.
tuna_data$m_prior <- c(-0.5, 1, 10)
## Add the `catch_cv_prior_rate` to `tuna_data`; required for the "explicit F"
## parameterizations. This is set to use a 90% probability that the CV is less
## than 0.05, which looks reasonable in prior predictive checks. Only used by
## the two "Explicit F" parameterizations.
tuna_data$catch_cv_prior <- cv_prior_rate(0.05, 0.9)
## Rate of the exponential prior on each F. A rate of 1 gives a Uniform(0, 1)
## prior on proportion of biomass removed by fishing each year. Only used by the
## two "Explicit F" parmameterizations.
tuna_data$F_prior <- 1

## Set the specifications for each chain. 30,000 iterations per chain with 5,000
## warmup iterations and 4 chains, so 100,000 post-warmup samples. Probably
## overkill, but better for detecting any sneaky divergent transitions.
chain_spec <- list(n_iter = 30e3,
                   n_warm = 5e3,
                   n_chain = 4L)

## Choose a range of `adapt_delta` values (default is 0.8)
adapt_delta_values <- c(seq(0.7, 0.95, 0.05), 0.975, 0.99, 0.999)
adapt_delta_values <- c(seq(0.8, 0.95, 0.05), 0.975, 0.99, 0.999)
## Increase max treedepth for the explicit F parameterizations. 20 is a
## substantial increase but may still finish in my lifetime. Wonder if this will
## allow better adaptation and so fewer divergences??
max_treedepth <- 20L

