## In order to avoid needing to recompile the model every run, first compile
## each model in global scope.
## <https://discourse.mc-stan.org/t/stan-recompile-to-avoid-r-from-crashing/2631/2>
Schaefer_centered <- stan_model("src/models/101_centered.stan")
Schaefer_ncproc <- stan_model("src/models/110_ncproc.stan")
Schaefer_margq <- stan_model("src/models/120_margq.stan")
Schaefer_exF <- stan_model("src/models/130_exF.stan")
Schaefer_exF_margq <- stan_model("src/models/132_exF_margq.stan")
Schaefer_constrP <- stan_model("src/models/140_constrP.stan")

## Fix the Pella-Tomlinson shape parameter to 2 for Schaefer dynamics
tuna_data$m <- 2

## Set up a data frame with each compiled model, its name, and initial value
## generator. This is used to cross with the `adapt_delta_values` vector to get
## all combinations. Here we can also include the "constrained P"
## parameterization, which depends on the quadratic nature of the Schaefer model
## to calculate its constraints.
Schaefer_models <- tribble(
  ~ model_name       , ~ model          ,      ~ init,
  "Centered"         ,  Schaefer_centered,   init_cent,
  "Noncentered"      ,    Schaefer_ncproc, init_ncproc,
  "Marginal q"       ,     Schaefer_margq,   init_cent,
  "Explicit F"       ,       Schaefer_exF,    init_exF,
  "Explicit F marg q", Schaefer_exF_margq,    init_exF,
  "Constrained P"    ,   Schaefer_constrP,    init_constrP)

## Sample from each of these models
Schaefer_fits <- cross_df(list(model_name = Schaefer_models$model_name,
                        adapt_delta = adapt_delta_values)) %>%
  left_join(Schaefer_models, by = "model_name") %>%
  mutate(fit = pmap(., ~ sampling(object = ..3,
                                  data = tuna_data,
                                  chains = chain_spec$n_chain,
                                  iter = chain_spec$n_iter,
                                  warmup = chain_spec$n_warm,
                                  init = ..4,
                                  control = list(adapt_delta = ..2))))

save(Schaefer_fits, chain_spec,
     file = "results/Schaefer_fits.Rdata")

