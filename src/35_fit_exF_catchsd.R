## Load packages and data
source("src/01_setup.R")
## Load plot specifics and initial value function
source("src/02_fitfns.R")

## Load previously fit posterior; use these samples as initial values for new
## fits
datetime <- "2018-12-05 15:49:31"
load(paste0("results/", datetime, "_fits.Rdata"))
post_exF <- rstan::extract(fit_df$fit[[8]])

set.seed(9448949)

## Same specifications as other fits; 2e4 should be enough to pick up
## pathologies and allow for comparison of posterior vales, and 5e3 warmup
## should give plenty of time for adaptation
chain_spec <- list(n_iter = 2e4,
                   n_warm = 5e3,
                   n_chain = 6L)

## Explicit F flattens posterior so much that it needs a larger treedepth
exF_control = list(max_treedepth = 15L)

## Standard deviations to try for catch, from 1e-2 to 100
catch_sd <- 10^(seq(-2, 2, length.out = 10L))

## Function to change the `catch_sd` of each data list
catch_sd_data <- function(catch_sd) {
  td2 <- tuna_data
  td2$catch_sd <- catch_sd
  td2
}

## Model location
modfile <- "src/models/30_exF.stan"

## Retrieve initialization values for each chain using previous model fit
init_exF <- make_init_list(chain_spec$n_chain, post_exF)

## Fit the models in a data frame
csd_df <- data_frame(catch_sd = catch_sd) %>%
  mutate(data = map(catch_sd, catch_sd_data),
         fit = map(data, ~ stan(file = modfile,
                                data = .x,
                                init = init_exF,
                                chains = chain_spec$n_chain,
                                iter = chain_spec$n_iter,
                                warmup = chain_spec$n_warm,
                                control = list(max_treedepth = 15L))))
## Do this so that the models can be serialized and then reloaded successfully
walk(csd_df$fit, function(f) f@stanmodel@dso <- new('cxxdso'))
## Save with a unique filename
save(csd_df, chain_spec,
     file = paste0("results/", Sys.time(), "_csd.Rdata"))

