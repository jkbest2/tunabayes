## Load packages and data
source("src/01_setup.R")
## Load plot specifics and initial value function
source("src/02_fitfns.R")

datetime <- "2018-12-05 15:49:31"
load(paste0("results/", datetime, "_fits.Rdata"))

set.seed(38493)

chain_spec <- list(n_iter = 1e4,
                   n_warm = 5e3,
                   n_chain = 4)

init_list <- list(list(r = rlnorm(1L, -1.28, 1 / sqrt(3.845)),
                       K = rlnorm(1L, 5.042905, 1 / sqrt(3.7603664)),
                       q = 0.28,
                       sigma2 = 1 / rgamma(1L, 3.785518, 0.010223),
                       tau2 = 1 / rgamma(1L, 1.708603, 0.008613854),
                       p = seq(1, 0.35, length.out = 23L)))

init_list <- list(list(r = 0.3,
                       K = 250,
                       q = 0.28,
                       sigma2 = 0.0015,
                       tau2 = 0.02,
                       P = seq(1, 0.35, length.out = 23L)))

init_fn <- function() {
  list(r = 0.3,
       K = 250,
       q = 0.28,
       sigma2 = 0.0015,
       tau2 = 0.02,
       P = seq(1, 0.35, length.out = 22))
}

fit <- stan("src/models_reject/01_centered.stan",
            data = tuna_data,
            chains = 1L,
            init = init_fn)

## Use previous fits to get initial values for these fits.
fit_df %>%
  mutate(post = rstan::extract(fit)) %>%
  transmute(init_list = )

## Control with increased `adapt_delta` to try to eliminate divergences
adj_control = list(adapt_delta = 0.975)
## Explicit F flattens posterior so much that it needs a larger treedepth
exF_control = list(max_treedepth = 15L)

mod_df <- tribble(
  ~ model_name            ,      ~ model_file,   ~ control, ~ init,
  "Truncated"             ,    "00_truncated",        NULL, 0,#"random",
  "Centered"              ,     "01_centered",        NULL, 0,#"random",
  "Centered adj"          ,     "01_centered", adj_control, 0,#"random",
  "Noncentered"           ,       "10_ncproc",        NULL, 0,#"random",
  "Noncentered adj"       ,       "10_ncproc", adj_control, 0,#"random",
  "Marginal q"            ,        "20_margq",        NULL, 0,#"random",
  "Marginal q adj"        ,        "20_margq", adj_control, 0,#"random",
  "Explicit F"            ,          "30_exF",        NULL, init_expF,
  "Explicit F adj"        ,          "30_exF", exF_control, init_expF,
  "Explicit F marg q"     ,          "30_exF",        NULL, init_expF,
  "Explicit F marg q adj" ,          "30_exF", exF_control, init_expF)

## Fit and save each of these models
fit_df <- mod_df %>%
  mutate(model_path = paste0("src/models_reject/",
                             model_file, ".stan")) %>%
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
     file = paste0("results/", Sys.time(), "_rejectfits.Rdata"))

