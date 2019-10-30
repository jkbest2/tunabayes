## Source common data, adapt_delta values, etc.
source("src/30_fitall.R")

## In order to avoid needing to recompile the model every run, first compile
## each model in global scope.
## <https://discourse.mc-stan.org/t/stan-recompile-to-avoid-r-from-crashing/2631/2>
fixedPT_centered <- stan_model("src/models/101_centered.stan")
fixedPT_ncproc <- stan_model("src/models/110_ncproc.stan")
fixedPT_margq <- stan_model("src/models/120_margq.stan")
fixedPT_exF <- stan_model("src/models/130_exF.stan")
fixedPT_exF_margq <- stan_model("src/models/132_exF_margq.stan")

## Fix the Pella-Tomlinson shape parameter to a value where MSY occurs at 40%
## depletion, as is commom for similar species.
tuna_data$m <- Pmsy_to_m(0.4)

## Set up a data frame with each compiled model, its name, and initial value
## generator. This is used to cross with the `adapt_delta_values` vector to get
## all combinations.
fixedPT_models <- tribble(
  ~ model_name       , ~ model          ,      ~ init,
  "Centered"         ,  fixedPT_centered,   init_cent,
  "Noncentered"      ,    fixedPT_ncproc, init_ncproc,
  "Marginal q"       ,     fixedPT_margq,   init_cent,
  "Explicit F"       ,       fixedPT_exF,    init_exF,
  "Explicit F marg q", fixedPT_exF_margq,    init_exF)

## Sample from each of these models
fixedPT_fits <- cross_df(list(model_name = fixedPT_models$model_name,
                        adapt_delta = adapt_delta_values)) %>%
  left_join(fixedPT_models, by = "model_name") %>%
  mutate(fit = pmap(., ~ sampling(object = ..3,
                                  data = tuna_data,
                                  chains = chain_spec$n_chain,
                                  iter = chain_spec$n_iter,
                                  warmup = chain_spec$n_warm,
                                  init = ..4,
                                  control = list(adapt_delta = ..2,
                                                 max_treedepth = max_treedepth))))

saveRDS(fixedPT_fits, "results/fixedPT_fits.Rds")

fit <- sampling(fixedPT_exF,
                data = tuna_data,
                chains = chain_spec$n_chain,
                iter = chain_spec$n_iter,
                warmup = chain_spec$n_warm,
                init = init_exF,
                control = list(adapt_delta = 0.99,
                               max_treedepth = 20L))
saveRDS(fit, "results/flatharvest_fit.Rds")
