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

fit_df <- tribble(~ model_name,        ~ model_file,   ~ control,
                   "Truncated",      "0_Schaefer_T",        NULL,
                    "Centered",      "1_Schaefer_C",        NULL,
           "Centered Adjusted",      "1_Schaefer_C", adj_control,
       "Noncentered Lognormal",     "3_Schaefer_PL",        NULL,
   "Noncentered Lognormal Adj",     "3_Schaefer_PL", adj_control,
          "Noncentered Normal",     "6_Schaefer_PN",        NULL,
      "Noncentered Normal Adj",     "6_Schaefer_PN", adj_control,
              "Marginalized q", "7_Schaefer_marg_q",        NULL,
          "Marginalized q Adj", "7_Schaefer_marg_q", adj_control) %>%
  mutate(model_path = paste0("src/models/", model_file, ".stan"),
         fit = map2(model_path, control, fitmod))
## Pretty sure this is necessary to save `stanfit` objects so that you don't get
## weird errors when they are read back into R
walk(fit_df$fit, function(f) f@stanmodel@dso <- new('cxxdso'))

save(fit_df, chain_spec,
     file = paste0("results/", Sys.time(), "_fits.Rdata"))

