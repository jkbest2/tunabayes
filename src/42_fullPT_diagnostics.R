source("src/40_postprocess.R")

load("results/fullPT_fits.Rdata")

fullPT_diagnostics <- diagnose_fits(fullPT_fits)

saveRDS(fullPT_diagnostics, "results/fullPT_diagnostics.rda")

