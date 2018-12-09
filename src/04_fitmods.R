## Load packages and data
source("src/01_setup.R")
## Load plot specifics and initial value function
source("src/02_fitfns.R")

set.seed(38493)

chain_spec <- list(n_iter = 2e4,
                   n_warm = 5e3,
                   n_chain = 6L)

## Control with increased `adapt_delta` to try to eliminate divergences
adj_control = list(adapt_delta = 0.975)
## Explicit F flattens posterior so much that it needs a larger treedepth
exF_control = list(max_treedepth = 15L)

mod_df <- tribble(
  ~ model_name            ,   ~ model_file,   ~ control,   ~ init,
  "Truncated"             , "00_truncated",        NULL, "random",
  "Centered"              ,  "01_centered",        NULL, "random",
  "Centered adj"          ,  "01_centered", adj_control, "random",
  "Noncentered"           ,    "10_ncproc",        NULL, "random",
  "Noncentered adj"       ,    "10_ncproc", adj_control, "random",
  "Marginal q"            ,     "20_margq",        NULL, "random",
  "Marginal q adj"        ,     "20_margq", adj_control, "random",
  "Explicit F"            ,       "30_exF",        NULL, init_expF,
  "Explicit F adj"        ,       "30_exF", exF_control, init_expF,
  "Explicit F marg q"     , "30_exF_margq",        NULL, init_expF,
  "Explicit F marg q adj" , "30_exF_margq", exF_control, init_expF,
  "Constrained P"         ,   "40_constrP",        NULL,  "random",
  "Constrained P adj"     ,   "40_constrP", adj_control,  "random")

## Fit and save each of these models
fit_df <- mod_df %>%
  mutate(model_path = paste0("src/models/", model_file, ".stan")) %>%
  mutate(fit = pmap(., ~ stan(file = ..5,
                              data = tuna_data,
                              chains = chain_spec$n_chain,
                              iter = chain_spec$n_iter,
                              warmup = chain_spec$n_warm,
                              init = ..4,
                              control = ..3)))
## Pretty sure this is necessary to save `stanfit` objects so that you don't get
## weird errors when they are read back into R
walk(fit_df$fit, function(f) f@stanmodel@dso <- new('cxxdso'))
save(fit_df, chain_spec,
     file = paste0("results/", Sys.time(), "_fits.Rdata"))

