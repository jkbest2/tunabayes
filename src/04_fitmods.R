source("src/01_setup.R")

set.seed(38493)

chain_spec <- list(n_iter = 1e4,
                   n_warm = 5e3,
                   n_chain = 4)

fitmod <- function(file, control) {
  stan(file = file,
       data = tuna_data,
       chains = chain_spec$n_chain,
       iter = chain_spec$n_iter,
       warmup = chain_spec$n_warm,
       control = control)
}

adj_control = list(adapt_delta = 0.975)
exF_control = list(max_treedepth = 15L)

mod_df <- tribble(  ~ model_name,      ~ model_file,   ~ control,
                     "Truncated",    "00_truncated",        NULL,
                      "Centered",     "01_centered",        NULL,
             "Centered Adjusted",     "01_centered", adj_control,
                   "Noncentered",       "10_ncproc",        NULL,
               "Noncentered Adj",       "10_ncproc", adj_control,
                "Marginalized q",        "20_margq",        NULL,
            "Marginalized q Adj",        "20_margq", adj_control,
    "Marginalized q noncentered", "21_margq_ncproc",        NULL,
"Marginalized q noncentered adj", "21_margq_ncproc", adj_control,
                    "Explicit F",          "30_exF", exF_control)

mod_df %>%
  mutate(model_path = paste0("src/models/", model_file, ".stan"),
         fit = map2(model_path, control, fitmod))
## Pretty sure this is necessary to save `stanfit` objects so that you don't get
## weird errors when they are read back into R
walk(fit_df$fit, function(f) f@stanmodel@dso <- new('cxxdso'))

save(fit_df, chain_spec,
     file = paste0("results/", Sys.time(), "_fits.Rdata"))

data_df <- data_frame(T = c(6, 12, 18, 23),
                      data = rep(list(tuna_data), 4)) %>%
  mutate(C = map2(data, T, ~ .x$C[1:.y]),
         I = map2(data, T, ~ .x$I[1:.y]),
         data = pmap(list(C, I, T),
                     ~ list(C = ..1, I = ..2, T = ..3))) %>%
  select(T, data)

short_df <- cross_df(list(model_name = fit_df$model_name,
                          T = c(6, 12, 18, 23))) %>%
  left_join(data_df, by = "T") %>%
  left_join(mod_df, by = "model_name") %>%
  mutate(model_path = paste0("src/models/", model_file, ".stan"),
         fit = pmap(list(model_path, data, control),
                    ~ stan(file = ..1, data = ..2,
                           chains = 4, iter = 4000, warmup = 2000,
                           control = ..3)))
walk(short_df$fit, function(f) f@stanmodel@dso <- new('cxxdso'))
save(fit_df, chain_spec,
     file = paste0("results/", Sys.time(), "_shortfits.Rdata"))
