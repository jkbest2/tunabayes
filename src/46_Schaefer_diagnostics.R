source("src/40_postprocess.R")

load("results/Schaefer_fits.Rdata")

Schaefer_diagnostics <- diagnose_fits(Schaefer_fits)

saveRDS(Schaefer_diagnostics, "results/Schaefer_diagnostics.rda")

