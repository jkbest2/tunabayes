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

mod_df <- tribble(~ model_name,          ~ model_file,   ~ control,
                   "Truncated",        "0_Schaefer_T",        NULL,
                    "Centered",        "1_Schaefer_C",        NULL,
           "Centered Adjusted",        "1_Schaefer_C", adj_control,
       "Noncentered Lognormal",       "3_Schaefer_PL",        NULL,
   "Noncentered Lognormal Adj",       "3_Schaefer_PL", adj_control,
          "Noncentered Normal",       "6_Schaefer_PN",        NULL,
      "Noncentered Normal Adj",       "6_Schaefer_PN", adj_control,
              "Marginalized q",   "7_Schaefer_marg_q",        NULL,
          "Marginalized q Adj",   "7_Schaefer_marg_q", adj_control,
          "Explicit catch add",    "8_Schaefer_exp_F",        NULL,
      "Explicit catch add adj",    "8_Schaefer_exp_F", adj_control,
            "Exp catch nc add", "9_Schaefer_exp_F_nc",        NULL,
        "Exp catch nc add adj", "9_Schaefer_exp_F_nc", adj_control)

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
