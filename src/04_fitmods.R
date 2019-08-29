## Load packages and data
source("src/01_setup.R")
## Load plot specifics and initial value function
source("src/02_fitfns.R")

## Add the `catch_cv_prior_rate` to `tuna_data`; required for the "explicit F"
## parameterizations. This is set to use a 90% probability that the CV is less
## than 0.05, which looks reasonable in prior predictive checks.
tuna_data$catch_cv_prior_rate <- cv_prior_rate(0.05, 0.9)

set.seed(38493)

## Set the specifications for each chain. Currently 20,000 total iterations,
## with 5,000 warmup iterations and 6 chains. Overkill, but better for detecting
## any sneaky divergent transitions.
chain_spec <- list(n_iter = 2e4,
                   n_warm = 5e3,
                   n_chain = 6L)

## Choose a range of `adapt_delta` values (default is 0.8)
adapt_delta_values <- c(seq(0.75, 0.95, 0.05), 0.975, 0.99)

## In order to avoid needing to recompile the model every run, first compile
## each model in global scope.
## <https://discourse.mc-stan.org/t/stan-recompile-to-avoid-r-from-crashing/2631/2>
mod_centered <- stan_model("src/models/01_centered.stan")
mod_ncproc <- stan_model("src/models/10_ncproc.stan")
mod_margq <- stan_model("src/models/20_margq.stan")
mod_exF <- stan_model("src/models/30_exF.stan")
mod_exF_margq <- stan_model("src/models/32_exF_margq.stan")

## Set up a data frame with each compiled model, its name, and initial value
## generator. This is used to cross with the `adapt_delta_values` vector to get
## all combinations.
model_df <- tribble(
  ~ model_name       , ~ model      ,      ~ init,
  "Centered"         ,  mod_centered,   init_cent,
  "Noncentered"      ,    mod_ncproc, init_ncproc,
  "Marginal q"       ,     mod_margq,   init_cent,
  "Explicit F"       ,       mod_exF,   init_expF,
  "Explicit F marg q", mod_exF_margq,   init_expF)

## Sample from each of these models
fit_df <- cross_df(list(model_name = model_df$model_name,
                        adapt_delta = adapt_delta_values)) %>%
  left_join(model_df, by = "model_name") %>%
  #########################################################################
  ## FIXME Only use next line when debugging! Doesn't fit all combinations!
  #########################################################################
  filter(adapt_delta == 0.80) %>%
  mutate(fit = pmap(., ~ sampling(object = ..3,
                                  data = tuna_data,
                                  chains = chain_spec$n_chain,
                                  iter = chain_spec$n_iter,
                                  warmup = chain_spec$n_warm,
                                  init = ..4,
                                  control = list(adapt_delta = ..2))))

save(fit_df, chain_spec,
     file = paste0("results/", Sys.time(), "adapt_delta_fits.Rdata"))

