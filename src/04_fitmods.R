## Load packages and data
source("src/01_setup.R")
## Load plot specifics and initial value function
source("src/02_fitfns.R")

set.seed(38493)

chain_spec <- list(n_iter = 1e4,
                   n_warm = 5e3,
                   n_chain = 4)

## Control with increased `adapt_delta` to try to eliminate divergences
adj_control = list(adapt_delta = 0.975)
## Explicit F flattens posterior so much that it needs a larger treedepth
exF_control = list(max_treedepth = 15L)

mod_df <- tribble(
  ~ model_name            ,      ~ model_file,   ~ control, ~ init,
  "Truncated"             ,    "00_truncated",        NULL, "random",
  "Centered"              ,     "01_centered",        NULL, "random",
  "Centered adj"          ,     "01_centered", adj_control, "random",
  "Noncentered"           ,       "10_ncproc",        NULL, "random",
  "Noncentered adj"       ,       "10_ncproc", adj_control, "random",
  "Marginal q"            ,        "20_margq",        NULL, "random",
  "Marginal q adj"        ,        "20_margq", adj_control, "random",
  "Marginal q noncen"     , "21_margq_ncproc",        NULL, "random",
  "Marginal q noncen adj" , "21_margq_ncproc", adj_control, "random",
  "Explicit F"            ,          "30_exF", exF_control, init_expF)

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

## Fit models with partial time series
## data_df <- data_frame(T = c(6, 12, 18, 23),
##                       data = rep(list(tuna_data), 4)) %>%
##   mutate(C = map2(data, T, ~ .x$C[1:.y]),
##          I = map2(data, T, ~ .x$I[1:.y]),
##          data = pmap(list(C, I, T),
##                      ~ list(C = ..1, I = ..2, T = ..3))) %>%
##   select(T, data)

## short_df <- cross_df(list(model_name = fit_df$model_name,
##                           T = c(6, 12, 18, 23))) %>%
##   left_join(data_df, by = "T") %>%
##   left_join(mod_df, by = "model_name") %>%
##   mutate(model_path = paste0("src/models/", model_file, ".stan"),
##          fit = pmap(list(model_path, data, control),
##                     ~ stan(file = ..1, data = ..2,
##                            chains = 4, iter = 4000, warmup = 2000,
##                            control = ..3)))
## walk(short_df$fit, function(f) f@stanmodel@dso <- new('cxxdso'))
## save(fit_df, chain_spec,
##      file = paste0("results/", Sys.time(), "_shortfits.Rdata"))
