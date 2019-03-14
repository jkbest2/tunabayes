## Load packages and data
source("src/01_setup.R")
## Load plot specifics and initial value function
source("src/02_fitfns.R")

set.seed(38493)

chain_spec <- list(n_iter = 2e4,
                   n_warm = 5e3,
                   n_chain = 6L)

## Control with increased `adapt_delta` to try to eliminate divergences
adj_ad = list(adapt_delta = 0.975)
## Explicit F flattens posterior so much that it needs a larger treedepth
adj_td = list(max_treedepth = 15L)

mod_df <- tribble(
  ~ model_name       , ~ adj,   ~ model_file, ~ control,       ~ init,
  "Truncated"        , FALSE, "00_truncated",      NULL,   init_trunc,
  "Truncated"        ,  TRUE, "00_truncated",    adj_ad,   init_trunc,
  "Centered"         , FALSE,  "01_centered",      NULL,    init_cent,
  "Centered"         ,  TRUE,  "01_centered",    adj_ad,    init_cent,
  "Noncentered"      , FALSE,    "10_ncproc",      NULL,  init_ncproc,
  "Noncentered"      ,  TRUE,    "10_ncproc",    adj_ad,  init_ncproc,
  "Marginal q"       , FALSE,     "20_margq",      NULL,   init_margq,
  "Marginal q"       ,  TRUE,     "20_margq",    adj_ad,   init_margq,
  "Explicit F"       , FALSE,       "30_exF",      NULL,    init_expF,
  "Explicit F"       ,  TRUE,       "30_exF",    adj_td,    init_expF,
  "Explicit F marg q", FALSE, "32_exF_margq",      NULL,    init_expF,
  "Explicit F marg q",  TRUE, "32_exF_margq",    adj_td,    init_expF,
  "Constrained P"    , FALSE,   "40_constrP",      NULL, init_constrP,
  "Constrained P"    ,  TRUE,   "40_constrP",    adj_ad, init_constrP)

## Fit and save each of these models
fit_df <- mod_df %>%
  mutate(model_path = paste0("src/models/", model_file, ".stan")) %>%
  mutate(fit = pmap(., ~ stan(file = ..6,
                              data = tuna_data,
                              chains = chain_spec$n_chain,
                              iter = chain_spec$n_iter,
                              warmup = chain_spec$n_warm,
                              init = ..5,
                              control = ..4)))
## Pretty sure this is necessary to save `stanfit` objects so that you don't get
## weird errors when they are read back into R
walk(fit_df$fit, function(f) f@stanmodel@dso <- new('cxxdso'))
save(fit_df, chain_spec,
     file = paste0("results/", Sys.time(), "_fits.Rdata"))

