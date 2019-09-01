source("src/40_postprocess.R")

load("results/fixedPT_fits.Rdata")

fixedPT_diagnostics <- diagnose_fits(fixedPT_fits)

saveRDS(fixedPT_diagnostics, "results/fixedPT_diagnostics.rda")

