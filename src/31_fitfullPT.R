## Source common data, adapt_delta values, etc.
source("src/30_fitall.R")

## In order to avoid needing to recompile the model every run, first compile
## each model in global scope.
## <https://discourse.mc-stan.org/t/stan-recompile-to-avoid-r-from-crashing/2631/2>
fullPT_centered <- stan_model("src/models/01_centered.stan")
fullPT_ncproc <- stan_model("src/models/10_ncproc.stan")
fullPT_margq <- stan_model("src/models/20_margq.stan")
fullPT_exF <- stan_model("src/models/30_exF.stan")
fullPT_exF_margq <- stan_model("src/models/32_exF_margq.stan")

## Set up a data frame with each compiled model, its name, and initial value
## generator. This is used to cross with the `adapt_delta_values` vector to get
## all combinations.
fullPT_models <- tribble(
  ~ model_name       ,  ~ model        ,      ~ init,
  "Centered"         ,  fullPT_centered,   init_cent,
  "Noncentered"      ,    fullPT_ncproc, init_ncproc,
  "Marginal q"       ,     fullPT_margq,   init_cent,
  "Explicit F"       ,       fullPT_exF,    init_exF,
  "Explicit F marg q", fullPT_exF_margq,    init_exF)

## Sample from each of these models
fullPT_fits <- cross_df(list(model_name = fullPT_models$model_name,
                        adapt_delta = adapt_delta_values)) %>%
  left_join(fullPT_models, by = "model_name") %>%
  mutate(fit = pmap(., ~ sampling(object = ..3,
                                  data = tuna_data,
                                  chains = chain_spec$n_chain,
                                  iter = chain_spec$n_iter,
                                  warmup = chain_spec$n_warm,
                                  init = ..4,
                                  control = list(adapt_delta = ..2))))

save(fullPT_fits, chain_spec,
     file = "results/fullPT_fits.Rdata")