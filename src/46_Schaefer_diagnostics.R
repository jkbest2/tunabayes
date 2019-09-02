source("src/40_postprocess.R")

Schaefer_fits <- readRDS("results/Schaefer_fits.Rds")

Schaefer_diagnostics <- diagnose_fits(Schaefer_fits)

saveRDS(Schaefer_diagnostics, "results/Schaefer_diagnostics.Rds")

